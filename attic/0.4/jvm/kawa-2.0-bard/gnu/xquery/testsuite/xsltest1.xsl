<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="chapter">
    <CHAPTER>
      <xsl:copy-of select='<foo/>'/>
      <xsl:text>  </xsl:text>-<xsl:value-of select='concat("<un", "known>")'/>
      <xsl:apply-templates/>
    </CHAPTER>
  </xsl:template>
</xsl:stylesheet>
