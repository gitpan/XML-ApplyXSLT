# $Id: Apache.pm,v 1.16 2006/07/12 02:39:48 jmates Exp $
#
# The author disclaims all copyrights and releases this module into the
# public domain.
#
# A mod_perl1 interface to XML::ApplyXSLT for the transformation of XML
# documents via XSLT.
#
# For more documentation, run perldoc(1) on this module.

package XML::ApplyXSLT::Apache;

use 5.005;
use strict;
use warnings;

our $VERSION = '0.16';

use Apache::Constants qw(:common REDIRECT);
use Apache::File ();
use Apache::Log  ();
use Apache::URI  ();

#use Date::Parse ();

use XML::ApplyXSLT ();
my $xapply = XML::ApplyXSLT->new;

# TODO allow different rules for different areas if need be?
# TODO cache mtime on rules file for if-modified calc below or reloading needs?
my $rules_file = Apache->server_root_relative('conf/applyxslt-rules');

if ( open my $rfh, "< $rules_file" ) {
  $xapply->rules($rfh);
} else {
  remark(
    'error',
    "could not load rules file",
    { errno => $!, file => $rules_file }
  );
}

# TODO way to set these from prefs?
$xapply->config_libxml(
  { load_ext_dtd => 0, expand_entities => 0, complete_attributes => 0 } );

sub handler {
  my $r = shift;

  # TODO prolly need a few DECLINES here or have a httpd prefs to avoid
  # certain areas or types, pre-file-and-rules-parse?
  #return DECLINED if not defined $r->content_type;

  my $file = $r->filename;
  my $uri  = Apache::URI->parse($r);

  #return DECLINED if $r->content_type() eq 'httpd/unix-directory';
  # KLUGE work around Apache internal redirect on bare directories
  if ($r->content_type() eq 'httpd/unix-directory' ) {
    if ( $r->uri =~ m{/$} ) {
      return DECLINED;
    } else {
      sleep 3;
      $r->headers_out->set(Location => 'http://sial.org' . $uri->path . '/' );
      return REDIRECT;
    }
  }

  my %request_params;
  my %request_defaults;

  my %param = $r->args || ();

  # set style from query string, if possible
  for my $param (%param) {
    $request_defaults{style} = $1
     if $param eq 'style'
     and $param{$param} =~ m/ ([A-Za-z0-9_-]+) /x;
  }

  my ($port) = $uri->port =~ m/ (\d+) /x;
  $port = 80 unless defined $port;
  $request_defaults{site} =
   $uri->scheme . '://' . $uri->hostname . ( $port != 80 ? ":$port" : '' );

  my $doc;
  unless ( $doc = $xapply->parse($file) ) {
    remark(
      'warn',
      'could not parse file',
      { errno => $xapply->errorstring, file => $file }
    );
    return DECLINED;
  }

  my ( $filedata, $defaults, $params ) =
   $xapply->study( $doc, $file, $r->document_root );

  unless ( defined $filedata ) {
    remark( 'warn', 'no filedata found', { file => $file } );
    return DECLINED;
  }

  $defaults = {} unless defined $defaults;
  $params   = {} unless defined $params;

  %$defaults = ( %$filedata, %$defaults, %request_defaults );

  # KLUGE nuke filename if DirectoryIndex name (currently index.xml) and
  # fix slashes so subdir can vanish without // problems
  $defaults->{filename} =~ s, index\.xml ,,x;
  $defaults->{filename} = '/' . $defaults->{filename};

  # macro expansion as well on XSL params
  %$params = ( %$params, %request_params );
  $params = $xapply->expand( $params, $defaults );

  my ( $docref, $details ) =
   $xapply->transform( $doc, default => $defaults, param => $params );
  unless ( defined $docref ) {
    remark(
      'error',
      'could not parse document',
      { errno => $xapply->errorstring, file => $file }
    );
    return DECLINED;
  }

  # TODO how handle output encoding?

  unless ( $details->{'media_type'} ) {
    remark( 'error', 'no Content-Type for results', { file => $file } );

    # TODO might need to return errors instead soas to prevent raw XML
    # from going at the user?
    return DECLINED;
  }

  $r->content_type( $details->{'media_type'} );
  $r->set_content_length(
    do { use bytes; length $$docref }
  );

  # TODO improve this, need to include mtime of rules and stylesheet ideally
  #  $r->update_mtime( (stat $r->finfo)[9] );
  #  $r->update_mtime(
  #    Date::Parse::str2time( substr q$Date: 2006/07/12 02:39:48 $, 6 ) );
  $r->set_last_modified( ( stat $r->finfo )[9] );

  # TODO load this from prefs somehow?
  if ( $r->protocol =~ /(\d\.\d)/ && $1 >= 1.1 ) {
    $r->header_out( 'Cache-Control', 'max-age=' . 7 * 24 * 60 * 60 );
  }

  # TODO do the etag stuff here? would need tp MD5 or similar off
  # of data such as: the document, the stylesheet, and possibly
  # other fields? (good for when have large or mainly static docs)
  if ( ( my $rc = $r->meets_conditions ) != OK ) {
    return $rc;
  }

  $r->send_http_header;
  return OK if $r->header_only;

  print $$docref;
  return OK;
}

sub remark {
  my $priority   = shift;
  my $message    = shift;
  my $attributes = shift;

  chomp $message;

  my $attr_str;
  if ($attributes) {
    $attr_str = join ', ',
     map { $attributes->{$_} ||= ''; "$_=$attributes->{$_}" }
     sort keys %$attributes;
  }

  my $r = Apache->request || Apache->server;
  $r->log->$priority( $message . ( $attr_str ? ": $attr_str" : '' ) );
  return 1;
}

1;
__END__

=head1 NAME

XML::ApplyXSLT::Apache - mod_perl1 interface to XML::ApplyXSLT

=head1 SYNOPSIS

  PerlModule XML::ApplyXSLT::Apache

  <Directory "/foo">
    SetHandler perl-script
    PerlHandler XML::ApplyXSLT::Apache
  </Directory>

=head1 DESCRIPTION

Apache (mod_perl1) interface to L<XML::ApplyXSLT|XML::ApplyXSLT>.

=head1 BUGS

=head2 Reporting Bugs

Newer versions of this module may be available from CPAN.

If the bug is in the latest version, send a report to the author.
Patches that fix problems or add new features are welcome.

=head2 Known Issues

No known issues, though see source for TODO and other comments.

=head1 SEE ALSO

L<AxKit|AxKit>, for more complex XML mangling needs.

The supporting modules L<XML::LibXML|XML::LibXML> and
L<XML::LibXSLT|XML::LibXSLT>.

=head1 AUTHOR

Jeremy Mates, E<lt>jmates@sial.orgE<gt>

=head1 COPYRIGHT AND LICENSE

The author disclaims all copyrights and releases this module into the
public domain.

=cut
