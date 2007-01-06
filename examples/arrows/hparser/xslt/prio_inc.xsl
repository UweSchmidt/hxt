<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="element_type2">
     prio_inc.xsl match_element_type2                  =====3=
     <xsl:apply-imports/>
  </xsl:template>

  <xsl:template match="nested_element">              
     prio_inc.xsl match_nested_element occurance 1
  </xsl:template>

  <xsl:template match="element_type3">
     prio_inc.xsl match_element_type3 occurance 2 (after being included)    =====7=
  </xsl:template>

</xsl:transform>