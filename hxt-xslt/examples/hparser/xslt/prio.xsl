<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


  <xsl:import href="prio_imp1.xsl"/>
  <xsl:import href="prio_imp2.xsl"/>

  <xsl:template match="element_type3">
     unreachable prio.xsl match_element_type3 occurance 2 (before include)
  </xsl:template>

  <xsl:include href="prio_inc.xsl"/>

  <xsl:template match="nested_element">                   
     prio.xsl match_nested_element occurance 2 (after include) =====6=
  </xsl:template>


</xsl:transform>