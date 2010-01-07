<!-- Works only if we allow XPath to select xmlns:* attributes... -->
<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="*">
    <xsl:param name="nsattrs" select="@xmlns:*"/>
    <xsl:copy>
      <xsl:copy-of select="@*|$nsattrs|@xmlns:*"/>
      <xsl:apply-templates>
        <xsl:with-param name="nsattrs" select="$nsattrs|@xmlns:*" />
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

</xsl:transform>