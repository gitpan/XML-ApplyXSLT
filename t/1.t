# $Id: 1.t,v 1.20 2004/07/20 00:55:02 jmates Exp $
#
# Initial "does it load and perform basic operations" tests

use Test::More 'no_plan';
BEGIN { use_ok('XML::ApplyXSLT') }

ok( defined $XML::ApplyXSLT::VERSION, '$VERSION defined' );

my $xapply = XML::ApplyXSLT->new;
isa_ok( $xapply, 'XML::ApplyXSLT' );

# fiddle with XML::LibXML and XSLT options
# this turns off DTD expansion, preventing slow processing due to some
# remote site being down or slow
ok( $xapply->config_libxml( { load_ext_dtd => 0, expand_entities => 0 } ),
  'Configure XML::LibXML' );
ok( $xapply->config_libxslt( { max_depth => 1000 } ),
  'Configure XML::LibXSLT' );

# a failed parse, to test errorstring
my $notxml = 'this string is not XML';
$response = $xapply->parse( \$notxml );
ok( !$response, 'XML parse failed on invalid XML' );

# this error message comes from module itself
cmp_ok( $xapply->errorstring, '=~', qr/could not parse/, 'errorstring is set' );

# test XML via different methods
my $xmlstring = '<xml/>';
ok( $xapply->parse( \$xmlstring ), 'parse XML string' )
 || diag $xapply->errorstring;

my $xmlfile = 't/x.xml';
ok( $xapply->parse($xmlfile), 'parse XML file' ) || diag $xapply->errorstring;

open FH, '< t/x.xml' or die "error: could not open file\n";
$response = $xapply->parse( \*FH );
ok( $response, 'parse XML filehandle' );
diag( $xapply->errorstring ) unless $response;

# when was this introduced?
# TODO skip test if not right perl for proper test counting if have plan
if ( $] > 5.008 ) {
  open my $fh, '< t/x.xml' or die "error: could not open file\n";
  $response = $xapply->parse($fh);
  ok( $response, 'parse XML filehandle by lexical' );
  diag( $xapply->errorstring ) unless $response;
}

# query routine allows arbitrary lookups, as opposed to easy
# routines for common parameters below (which currently use
# the query function internally)
my $result = $xapply->query_xpath( $response, '/*' );
isa_ok( $result, 'XML::LibXML::NodeList' ) || diag $xapply->errorstring;

my $filedata;
ok( $filedata = $xapply->filedata('t/x.xml'), 'get filedata for t/x.xml' );
cmp_ok( $filedata->{suffix},   'eq', 'xml',   'check filedata suffix' );
cmp_ok( $filedata->{file},     'eq', 'x',     'check filedata file field' );
cmp_ok( $filedata->{filename}, 'eq', 'x.xml', 'check filedata filename' );
ok( exists $filedata->{dirname}, 'filedata dirname exists' );

my $headers = $xapply->docdata($response);
ok( ref $headers eq 'HASH', 'docdata should return hash reference' )
 || diag $xapply->errorstring;
cmp_ok( $headers->{'rootname'}, 'eq', 'xml', 'Root element name' );
cmp_ok( $headers->{'pi'}->[0],
  'eq', 'applyxslt', 'Processing Instruction name' );

# also need to test document that has multiple PI, DOCTYPE strings
{
  my $pifile = 't/pi.xml';
  my $doc;
  ok( $doc = $xapply->parse($pifile) ) || diag $xapply->errorstring;

  my $headers;
  ok( $headers = $xapply->docdata($doc), 'get docdata' )
   || diag $xapply->errorstring;

  my @pi = qw(applyxslt secondpi);
  ok(
    eq_array( $headers->{'pi'}, \@pi ),
    'Check multiple processing instructions'
  );

  # pi.xml uses SYSTEM DTD...
  cmp_ok( $headers->{doctype_uri}, 'eq', 'xml.dtd', 'DOCTYPE SYSTEM check' );
}

# check PUBLIC DOCTYPE
{
  my $doc;
  ok( $doc = $xapply->parse('t/test.html'), 'parse t/test.html' );
  my $headers;
  ok( $headers = $xapply->docdata($doc), 'get docdata' );

  ok(
    eq_hash(
      $headers,
      {
        rootname    => 'html',
        doctype     => '-//W3C//DTD XHTML 1.1//EN',
        doctype_uri => 'http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd'
      }
    ),
    'Check DOCTYPE PUBLIC and other details'
  );
}

my $path = "style/%{class}/%{style}.xsl";

# test setting, getting of preference values
$xapply->config( { path => $path, class => 'test', style => 'default' } );
my $data = $xapply->config();
cmp_ok( $data->{'path'}, 'eq', $path, 'Read preferences back out' );

ok( open( RULES, '< t/rules' ), 'open rules file' );
ok( $xapply->rules( \*RULES ), 'load rules' );

$response = $xapply->parse('t/x.xml') || diag $xapply->errorstring;

my ( $docdetails, $defaults, $params );
ok( ( $docdetails, $defaults, $params ) =
   $xapply->study( $response, 't/x.xml' ) )
 || diag $xapply->errorstring;
ok( eq_hash( $defaults, { class => 'test', style => 'default' } ),
  'check study results' );

{
  my ( $docref, $details ) = $xapply->transform($response);

  cmp_ok( $details->{'encoding'},
    'eq', 'UTF-8', 'Transformation output encoding' )
   || diag $xapply->errorstring;
  cmp_ok( $details->{'media_type'},
    'eq', 'text/xml', 'Transformation output media type' );
}

{
  $xapply->config( { style => 'broken' } );
  my $docref = $xapply->transform;
  cmp_ok( $docref, 'eq', undef, 'Transformation failure check' )
   || diag $xapply->errorstring;
}

{
  $xapply->config( { style => 'nosuchstylefile' } );
  my $docref = $xapply->transform($response);
  cmp_ok( $docref, 'eq', undef, 'Transformation failure check' )
   || diag $xapply->errorstring;
}

{
  my $param_value = 'wanted';
  my $docref      = $xapply->transform(
    $response,
    default => { style     => 'param' },
    param   => { testparam => $param_value }
  );
  cmp_ok( $$docref, 'eq', $param_value, 'Pass through parameter value' );
}
