<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:import href="attset_imp.xsl" />

<xsl:attribute-set name="as1">
  <xsl:attribute name="att1">att1GLOB1</xsl:attribute>
  <xsl:attribute name="att1_1">att1_1Glob1</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="as1">
  <xsl:attribute name="att1">att1GLOB2</xsl:attribute>
</xsl:attribute-set>

</xsl:transform>
