<?xml version="1.0" encoding="utf8"?>
<!-- $Id: param.xsl,v 1.2 2004/06/12 06:59:14 jmates Exp $ -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text"/>
  <xsl:param name="testparam"/>
  <xsl:template match="/">
    <xsl:value-of select="$testparam"/>
  </xsl:template>
</xsl:stylesheet>
