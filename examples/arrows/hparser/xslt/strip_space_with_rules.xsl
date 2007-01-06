<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- <xsl:strip-space elements="element_type2" /> -->


<xsl:template match="element_type2" mode="strip-space">
  <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:apply-templates mode="strip-space">
      <xsl:with-param name="strip" select="true()" />
    </xsl:apply-templates>
  </xsl:copy>
</xsl:template>


<xsl:template match="*|/" mode="strip-space" priority="-1000">
  <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:apply-templates mode="strip-space"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="text()" mode="strip-space">
  <xsl:param name="strip" select="false()"/>
  <xsl:choose>
     <xsl:when test="$strip and normalize-space()=''">
     </xsl:when>
     <xsl:otherwise>
        <xsl:copy/>
     </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="processing-instruction()|comment()" mode="strip-space">
  <xsl:copy />
</xsl:template>


<xsl:template match="/">
===================1)
  <xsl:copy-of select="document" />
===================2)
  <xsl:apply-templates mode="strip-space" />

</xsl:template>


</xsl:transform>
