<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/|*">
    <xsl:copy use-attribute-sets="as">
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
  <xsl:attribute-set name="as">
    <xsl:attribute name="processed">TRUE</xsl:attribute>
  </xsl:attribute-set>
</xsl:transform>