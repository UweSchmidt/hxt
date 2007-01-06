<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:import href="prio_imp1_1.xsl"/>
<xsl:import href="prio_imp1_2.xsl"/>

<xsl:template match="document">
  unreachable (prio_imp1.xsl) match_document
</xsl:template>

  <xsl:template match="element_type2">
     prio_imp1.xsl match_element_type2                  =====4=
     <xsl:apply-imports/>
  </xsl:template>


  <xsl:template match="text()">
     <xsl:call-template name="handleText"/>
  </xsl:template>


</xsl:transform>