# $Id: ApplyXSLT.pm,v 1.35 2004/09/26 18:27:23 jmates Exp $
#
# The author disclaims all copyrights and releases this module into the
# public domain.
#
# Convert XML data with XSLT stylesheet files.
#
# For more documentation, run perldoc(1) on this module.

package XML::ApplyXSLT;

use 5.005;
use strict;
use warnings;

use base qw(Exporter);

use File::Basename qw(fileparse);
use File::Spec ();

use XML::LibXML  ();
use XML::LibXSLT ();

our $VERSION = '0.42';

my $suffix_char = '.';
my $suffix_re   = qr/(?<!^)\./;

# create object, setup XML and XSLT objects, accept prefs
sub new {
  my $class  = shift;
  my %params = @_;

  my $self = bless {}, $class;

  # TODO validation on this input?
  $self->{default} = $params{default} || {};

  # TODO better time to init these?
  $self->{xmlp} = XML::LibXML->new;
  $self->{xslp} = XML::LibXSLT->new;

  $self->rules( $params{rules} ) if exists $params{rules};

  return $self;
}

# alter or return prefs
sub config {
  my $self    = shift;
  my $default = shift || return $self->{default};

  $self->{default} = { %{ $self->{default} }, %$default };

  return 1;
}

# interface to configure XML::LibXML
sub config_libxml {
  my $self  = shift;
  my $prefs = shift || return;

  my %allowed;
  @allowed{
    qw(
     validation
     recover
     expand_entities
     keep_blanks
     pedantic_parser
     line_numbers
     load_ext_dtd
     complete_attributes
     expand_xinclude
     )
   } = ();

  for my $method ( grep exists $allowed{$_}, keys %$prefs ) {
    $self->{xmlp}->$method( $prefs->{$method} );
  }

  return 1;
}

sub config_libxslt {
  my $self  = shift;
  my $prefs = shift || return;

  my %allowed;
  @allowed{
    qw(
     max_depth
     debug_callback
     register_function
     )
   } = ();

  for my $method ( grep exists $allowed{$_}, keys %$prefs ) {
    $self->{xslp}->$method( $prefs->{$method} );
  }

  return 1;
}

# TODO fix/improve logging e.g. so can have warnings?

# diagnostic message accessors
sub errorstring {
  my $self = shift;
  return $self->{errorstring};
}

sub debugstring {
  my $self = shift;
  return $self->{debugstring};
}

# parses XML file by filehandle, scalar string, or filename. returns
# reference to rendered XML document.
sub parse {
  my $self = shift;

  $self->{errorstring} = '';
  $self->{debugstring} = '';

  # Could be a file, filehandle, or string. Figure out what to do.
  # Using IO::Wrap might be a portability win, though the XML::LibXML
  # docs say the C library layer can suck in files by name much faster
  # than Perl.
  my $what = shift;

  my %refmap = (
    GLOB   => 'parse_fh',
    SCALAR => 'parse_string',
    ''     => 'parse_file'
  );
  my $method = $refmap{ ref $what };
  unless ( defined $method ) {
    $self->{errorstring} = 'no parse method found';
    return;
  }

  my $doc;
  eval {
    $doc = $self->{xmlp}->$method( $method eq 'parse_string' ? $$what : $what );
  };
  if ($@) {
    chomp $@;
    $self->{debugstring} = $@;
    $self->{errorstring} = 'could not parse XML';
    return;
  }

  return $doc;
}

# load or return rules used to determine how to handle a particular XML
# document via file data or parsed document data
sub rules {
  my $self = shift;
  my $fh   = shift || return $self->{rules};

  # TODO more sanity checking?
  my ( $line, @rules );
 RULE: while ( $line = <$fh> ) {
    next if $line =~ /^\s*$/;
    $line =~ s/^\s+//;
    next if $line =~ /^#/;
    chomp $line;
    $line =~ s/\s+$//;

    # extend backslashed lines to include subseqent lines
    if ( $line =~ s/ \\ $ //x ) {
      $line .= <$fh>;
      redo RULE unless eof;
    }

    my @tokens;
   UBLE: {
      # non-quoted strings, backslashed quotes and whitespace allowed
      push( @tokens, $1 ), redo UBLE
       if $line =~ m/ \G ( [^"'\s]+ ) \s* /cgx;

      # single or double-quoted strings, backslashed quotes allowed
      push( @tokens, $2 ), redo UBLE
       if $line =~ m/ \G (['"]) ((?: \\.|[^\\\1] )+) \1 \s* /cgx;

      last UBLE if $line =~ / \G $ /gcx;

      # get here on bogus lines that above miss
      #
      # TODO need better error system so can flag warnings and attempt
      # to move on?
      $self->{errorstring} = "invalid rule at line $.";
      return;
    }

    next RULE unless @tokens >= 2;

    # unescape things like "\ " or "\n"
    @tokens = map { s/(\\.)/qq!"$1"!/eeg; $_ } @tokens;

    my %rule;
    $rule{action} = 'continue';

    # test subjects may not end in :, while defaults: or params: do
    if ( $tokens[0] !~ m/:$/ ) {

      $rule{subject} = shift @tokens;
      if ( $tokens[0] eq 'not' ) {
        $rule{negate} = 1;
        shift @tokens;
      }

      next RULE unless @tokens >= 2;

      $rule{operator} = shift @tokens;
      $rule{value}    = shift @tokens;

      if (
        @tokens
        and ($tokens[0] eq 'stop'
          or $tokens[0] eq 'ignore'
          or $tokens[0] eq 'continue' )
       ) {
        $rule{action} = shift @tokens;
      }
    }

    # deal with defaults: or params: that get set by rules, either
    # by setting where key=value pairs go, or parsing said pairs
    if (@tokens) {
      my $target = 'default';

      for my $token (@tokens) {
        if ( $token =~ m/^(default|param)s?:$/ ) {
          $target = $1;
        } else {
          my ( $k, $v ) = $token =~ m/^ ([\w.-]+) = (.*) $/x;
          $rule{$target}->{$k} = $v if defined $k;
        }
      }
    }
    push @rules, \%rule;
  }

  $self->{rules} = \@rules;
  return 1;
}

sub apply_rules {
  my $self    = shift;
  my $subject = shift;

  my %default;
  my %param;

 RULE: for my $rule ( @{ $self->{rules} } ) {
    my $topic = $rule->{subject};

    # test free rules can set defaults
    unless ( defined $topic
      and exists $subject->{$topic}
      and defined $subject->{$topic} ) {
      %default = ( %default, %{ $rule->{default} } )
       if exists $rule->{default};
      %param = ( %param, %{ $rule->{param} } )
       if exists $rule->{param};

      next RULE;
    }

    my $match    = 0;
    my $consider =
     ref $subject->{$topic} eq 'ARRAY'
     ? $subject->{$topic}
     : [ $subject->{$topic} ];

    if ( $rule->{operator} eq 'eq' ) {
      for my $thingy (@$consider) {
        if ( $thingy eq $rule->{value} ) {
          $match = 1;
          last;
        }
      }
    } elsif ( $rule->{operator} eq 'sub' ) {
      for my $thingy (@$consider) {
        if ( -1 < index $thingy, $rule->{value} ) {
          $match = 1;
          last;
        }
      }
    } else {
      warn "error: unknown operator for rule number ...\n";
      next RULE;
    }

    $match = $match ? 0 : 1 if exists $rule->{negate};
    next RULE unless $match;

    return if $rule->{action} eq 'ignore';

    # also set these on rule hits
    %default = ( %default, %{ $rule->{default} } )
     if exists $rule->{default};
    %param = ( %param, %{ $rule->{param} } )
     if exists $rule->{param};

    return \%default, \%param if $rule->{action} eq 'stop';
  }

  # oops, dropped off end of ruleset without being handled
  # default to "do not handle" in such case
  return;
}

# parses out various file information such as the dirname, filename,
# file name without suffix, suffix. Used by rules to figure out what to
# do with a particular file.
sub filedata {
  my $self     = shift;
  my $filename = shift;
  my $parent   = shift;

  my %filedata;

  ( $filedata{filename}, $filedata{dirname}, undef ) = fileparse $filename;

  $filedata{dirname} = File::Spec->rel2abs( $filedata{dirname} );

  # try to determine "subdir" and "parentdir" for possible chroot or URI
  # based work
  if ( defined $parent ) {
    $parent =~ s,/+$,,;

    my $offset = index $filedata{dirname}, $parent;
    if ( $offset > -1 ) {
      $filedata{subdir} = substr $filedata{dirname}, $offset + length $parent;

      $filedata{parentdir} = $parent;
    }
  }

  my @portions = split /$suffix_re/, $filedata{filename};
  $filedata{file} = $portions[0];
  if ( @portions > 1 ) {
    local $" = $suffix_char;
    $filedata{suffix} = "@portions[1..$#portions]";
  }
  return \%filedata;
}

# builds up a hash of document data such as DOCTYPE info and the root
# element name for determination of what class and style the document
# should be classified as. Returns hash references.
sub docdata {
  my $self = shift;
  my $doc  = shift;

  my %docdata;
  my $root = $doc->documentElement;
  if ($root) {
    $docdata{rootname} = $root->nodeName;
  }

  # TODO difference between internal and external here relevant?
  #my $doctype = $doc->externalSubset;
  my $doctype = $doc->internalSubset;

  if ($doctype) {
    # grr, XML::LibXML has incomplete Dtd handling at present, so have
    # to parse it manually
    my ( $ExternalID, $literal, $optional ) = $doctype->toString =~ m/^
     \s* <!DOCTYPE \s+  $docdata{rootname} \s+
     ( PUBLIC | SYSTEM ) \s+
     "([^"]+)"
     (?: \s+ "([^"]+)" )?/sx;

    $ExternalID ||= '';

    if ( $ExternalID eq 'PUBLIC' ) {
      $docdata{doctype} = $literal;
      $docdata{doctype_uri} = $optional || '';
    } elsif ( $ExternalID eq 'SYSTEM' ) {
      $docdata{doctype_uri} = $literal;
    }
  }

  my $pi_nodes = $self->query_xpath( $doc, 'processing-instruction()' );
  if ($pi_nodes) {
    my @pi_names;
    for my $node ( $pi_nodes->get_nodelist() ) {
      push @pi_names, $node->nodeName();
    }
    $docdata{pi} = \@pi_names if @pi_names;
  }

  return \%docdata;
}

# execute arbitrary XPath against document via findnodes()
sub query_xpath {
  my $self  = shift;
  my $doc   = shift;
  my $query = shift;

  $self->{errorstring} = '';
  $self->{debugstring} = '';

  my $results;
  eval { $results = $doc->findnodes($query); };
  if ($@) {
    chomp $@;
    $self->{debugstring} = $@;
    $self->{errorstring} = 'xpath query error';
    return;
  }

  return $results;
}

sub study {
  my $self = shift;
  my $doc  = shift;

  # TODO replace these with param-from-hash to be more like other methods?
  my $filename = shift;
  my $parent   = shift;

  # merge file and XML document metadata for rule tests
  my $filedata = defined $filename ? $self->filedata( $filename, $parent ) : {};
  %$filedata = ( %$filedata, %{ $self->docdata($doc) } );

  return $filedata, $self->apply_rules($filedata);
}

# needs to return style "id" for caching, and then something suitable to
# be fed to the parse routine (filename, handle, etc.)
sub get_style {
  my $self    = shift;
  my $default = shift || {};

  %$default = ( %{ $self->{default} }, %$default );

  unless ( exists $default->{path} and defined $default->{path} ) {
    $self->{errorstring} = 'no style path set';
    return;
  }

  my $style_doc = $self->expand( $default->{path}, $default );

  return $style_doc, $style_doc;
}

sub expand {
  my $self      = shift;
  my $something = shift;
  my $lookup    = shift;
  my $default   = shift;

  $default = '' unless defined $default;

  # bleh...
  my $what = ref $something;
  if ( $what eq 'HASH' ) {
    for my $value ( values %$something ) {
      $value =~ s/ %{ (\w+) } / $lookup->{$1} || $default /egx;
    }
  } elsif ( $what eq 'ARRAY' ) {
    for my $value (@$something) {
      $value =~ s/ %{ (\w+) } / $lookup->{$1} || $default /egx;
    }
  } else {
    $something =~ s/ %{ (\w+) } / $lookup->{$1} || $default /egx;
  }

  return $something;
}

# translate previously parsed XML document with stylesheet looked up via
# get_style method
sub transform {
  my $self   = shift;
  my $doc    = shift;
  my %params = @_;

  $self->{errorstring} = '';
  $self->{debugstring} = '';

  my ( $id, $style_doc ) = $self->get_style( $params{default} );
  return unless $id and $style_doc;

  my $stylesheet = $self->{style_cache}->{$id};

  # TODO support for refresh when ondisk more recent, and support to
  # remove from cache if have too many stylesheets in memory?
  unless ( defined $stylesheet
    and ref $stylesheet eq 'XML::LibXSLT::Stylesheet' ) {

    # geh, what if not a file in the future?  TODO move elsewhere
    unless ( -f $style_doc ) {
      $self->{errorstring} = "stylesheet not found: file=$style_doc";
      return;
    }

    my $docref = $self->parse($style_doc);
    return unless $docref;

    eval { $stylesheet = $self->{xslp}->parse_stylesheet($docref); };
    if ($@) {
      chomp $@;
      $self->{debugstring} = $@;
      $self->{errorstring} = 'could not parse XSLT stylesheet';
      return;
    }
    unless ( defined $stylesheet
      and ref $stylesheet eq 'XML::LibXSLT::Stylesheet' ) {
      $self->{errorstring} = 'stylesheet not a XML::LibXSLT::Stylesheet';
      return;
    }
  }

  my $results;
  eval {
    $results = $stylesheet->transform( $doc,
      keys %{ $params{param} }
      ? XML::LibXSLT::xpath_to_string( %{ $params{param} } )
      : () )
  };
  if ($@) {
    chomp $@;
    $self->{debugstring} = $@;
    $self->{errorstring} = 'could not transform XML document with stylesheet';
    return;
  }

  my $rendered = \$stylesheet->output_string($results);

  my %details;
  $details{encoding}   = $stylesheet->output_encoding;
  $details{media_type} = $stylesheet->media_type;

  return wantarray ? ( $rendered, \%details ) : $rendered;
}

1;
__END__

=head1 NAME

XML::ApplyXSLT - convert XML data with XSLT stylesheet files

=head1 SYNOPSIS

  use XML::ApplyXSLT;
  $xapply = XML::ApplyXSLT->new;

  # parse an XML document by various means
  $doc = $xapply->parse( $xml_filename ) || die $xapply->errorstring;
  $doc = $xapply->parse( \$xml_string );
  $doc = $xapply->parse( \*FILEHANDLE );

  # set global defaults, such as path, style, and class to lookup
  # stylesheets from the filesystem
  $xapply->config({ 
    class => 'test',
    style => 'default'
  });

  # load rules
  $xapply->rules( $rules_filehandle );

  # determine information about a given document (via rules)
  ( $filedata, $defaults, $params ) = $xapply->study( $doc, $xml_filename );

  # extra code here to mess with defaults, check parameters, etc.

  # transform the previously parsed XML document via stylesheet found
  # via path, class, and style lookups
  ( $docref, $details ) = $xapply->transform( $doc, 
    default => $defaults, param => $params);
  print $$docref;

=head1 DESCRIPTION

This module converts XML documents with XSLT files. As different
stylesheets could be applied to a particular XML format depending on the
context, methods are provided to determine what C<class> and C<style>
the XML data belongs to by C<DOCTYPE>, the root element name, or
Processing Instructions. The C<class> and C<style> information is used
to construct a path to a stylesheet file residing somewhere on the
filesystem, which is loaded and used to convert the XML data. The
C<class> and C<style> information can also be set manually, if only a
single stylesheet will be used.

Stylesheets are parsed and stored in memory to avoid reparsing the same
stylesheet for multiple XML documents.

The XML::LibXML and XML::LibXSLT modules provide XML and XSLT parsing.

As this is a new module, the methods may change as more is learned about
the needs of command line, CGI, or mod_perl based interfaces.

=head1 METHODS

Looking at the code in t/1.t probably best bet for usage at this point.

=over 4

=item B<new>

=item B<config>

Accepts hash to alter 'default' used to hold path, class, and style (or
anything else) information, or returns said default hash by reference.

=item B<config_libxml>

Passes configuration methods by hash to XML::LibXML object.

=item B<config_libxslt>

Passes configuration methods by hash to XML::LibXSLT object.

=item B<errorstring>

Last error message from module.

=item B<debugstring>

May contain XML::LibXML or XML::LibXSLT error data, which could include
unsafe characters or sensitive file data due to parse failures.

=item B<parse>

Parses XML document by filename, string, or filehandle. Returns
XML::LibXML object.

=item B<filedata>

Returns information about a filename. See also B<docdata>. Called
by B<study>.

=item B<docdata>

Parses XML document information, such as the root name and DOCTYPE
identification strings. See also B<filedata>. Called by B<study>.

=item B<rules>

Accepts filehandle to load rules from. These rules are used by B<study>.

=item B<apply_rules>

Internal routine used by B<style> to apply rules to document data.

=item B<study>

Runs B<filedata> and B<docdata> information past rules to figure out how
to classify the XML document in question.

=item B<expand>

Expands C<%{keyword}> style statements in passed data against a hash of
lookup values. Used by B<get_style> to expand the C<path> default to
find the location of the stylesheet to use.

=item B<transform>

Transforms XML::LibXML document with XML:::LibXSLT after looking up the
stylesheet to use via B<get_style> based of off 'default' data.

=item B<query_xpath>

Internal routine, used by B<docdata>.

=item B<get_style>

Internal, for stylesheet lookup.

=back

=head1 RULES

Rules are used to classify and set defaults and parameter values for XML
documents, based on the XML document info. A rule is a single line, and
may be extended by placing a backslash at the end of the line. Lines
beginning with an octothorpe (#) will be ignored, as will blank lines.
An example set of rules follow.

  # set a default for everything
  defaults: path=/var/www/htdocs/xsl/%{class}/%{style}.xsl \
    style=default

  # when in specific subdirectory, alter path default
  dirname sub "site/example.org/htdocs" \
    path=/var/www/site/example.org/htdocs/xsl/%{class}/%{style}.xsl

  # handle different document types
  rootname eq "eolas" stop \
    defaults: class=eolas \
    params: request.preferred_style=%{style}

  rootname eq "changelog" stop class=cvs2cl \
    params: request.preferred_style=%{style}

  doctype eq "-//OASIS//DTD DocBook XML V4.2//EN" stop class=docbook42

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

http://www.w3.org/TR/REC-xml

http://www.w3.org/TR/xslt

=head1 AUTHOR

Jeremy Mates, E<lt>jmates@sial.orgE<gt>

=head1 COPYRIGHT AND LICENSE

The author disclaims all copyrights and releases this module into the
public domain.

=cut
