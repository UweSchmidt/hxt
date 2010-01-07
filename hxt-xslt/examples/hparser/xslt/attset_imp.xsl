<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:attribute-set name="as1">
  <xsl:attribute name="att1">att1Loc1</xsl:attribute>
  <xsl:attribute name="att1_1">att1_1Loc1</xsl:attribute>
  <xsl:attribute name="att1_2">att1_2Loc1</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="as2" use-attribute-sets = "as1">
  <xsl:attribute name="att2">att2Loc1</xsl:attribute>
</xsl:attribute-set>

<xsl:template match="/">
  <LRE xsl:use-attribute-sets="as2" kyr="bar" kar="bra" kur="rab"/>
</xsl:template>

</xsl:transform>
