<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="/">
    <result>
     <xsl:apply-templates/>
    </result>
  </xsl:template>

  <xsl:template match="/" priority="0.5">
    <resultXXX>
     <xsl:apply-templates/>
    </resultXXX>
  </xsl:template>


  <xsl:template match="nested_element">
     <nested_element_matched>
     Attributes matched with default rules:
     <xsl:apply-templates select="@*"/>


     Attributes matched with default rules in mode m:
     <xsl:apply-templates select="@*" mode="m"/>

     Match content with default rules
     <xsl:apply-templates/>

     </nested_element_matched>
  </xsl:template>

</xsl:transform>
