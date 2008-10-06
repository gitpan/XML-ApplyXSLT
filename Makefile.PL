# $Id: Makefile.PL,v 1.3 2004/10/10 18:09:25 jmates Exp $

use 5.005;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'XML::ApplyXSLT',
    VERSION_FROM      => 'lib/XML/ApplyXSLT.pm',
    PREREQ_PM         => { 'XML::LibXML' => 1.53, 'XML::LibXSLT' => 1.50 },
    ($] >= 5.005 ?
      (ABSTRACT_FROM  => 'lib/XML/ApplyXSLT.pm',
       AUTHOR         => 'Jeremy Mates <jmates@sial.org>') : ()),
);
