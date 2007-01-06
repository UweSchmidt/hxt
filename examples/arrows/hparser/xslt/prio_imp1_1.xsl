<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">                      
  <result>
    prio_imp1_1.xsl match /           =====1= 
    <xsl:apply-templates />
  </result>
</xsl:template>

<xsl:template match="document">
  unreachable (prio_imp1_1.xsl) match_document
</xsl:template>


</xsl:transform>