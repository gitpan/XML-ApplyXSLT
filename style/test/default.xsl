<?xml version="1.0" encoding="utf8"?>
<!-- $Id: default.xsl,v 1.3 2004/06/10 05:39:43 jmates Exp $ -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="node()|@*|comment()|processing-instruction()">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*|comment()|processing-instruction()"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
