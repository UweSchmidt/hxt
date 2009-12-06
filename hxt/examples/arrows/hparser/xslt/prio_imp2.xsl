<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="document" priority="2.0">
  unreachable prio_imp2.xsl match_document Prio 2.0 occurance 1
</xsl:template>
<xsl:template match="document" priority="2.0">                
  prio_imp2.xsl match_document Prio 2.0 occurance 2    =====2=
  <xsl:apply-templates/>
</xsl:template>
<xsl:template match="document" priority="1.0">
  unreachable prio_imp2.xsl match_document Prio 1.0 occurance 1
</xsl:template>
<xsl:template match="document" priority="1.0">
  unreachable prio_imp2.xsl match_document Prio 1.0 occurance 2
</xsl:template>





</xsl:transform>