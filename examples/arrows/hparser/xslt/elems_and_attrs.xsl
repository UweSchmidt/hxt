<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <xsl:element name="result">
     <xsl:attribute name="attr">value</xsl:attribute>
     
      <xsl:element name="nested_{-2+3}">
          <xsl:attribute name="attr_nested">
              <xsl:value-of select="concat('value ',-5+4*4)" />
              <xsl:text>still within attr</xsl:text>
          </xsl:attribute>
          <xsl:text>textnode</xsl:text>
      </xsl:element>
    
  </xsl:element>
</xsl:template>

</xsl:transform>