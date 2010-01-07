<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="*">
    <xsl:param name="strip" select="true()"/>
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates>
        <xsl:with-param name="strip" 
           select="    ( local-name()!='text' or 
                         namespace-uri()!='http://www.w3.org/1999/XSL/Transform' )
                   and not(@xml:space='preserve')
                   and ($strip or @xml:space='default') "/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:param name="strip" select="true()"/>
    <xsl:if test="not($strip) or string-length(normalize-space(.)) &gt; 0">
      <xsl:value-of select="." />
    </xsl:if>
  </xsl:template>

</xsl:transform>