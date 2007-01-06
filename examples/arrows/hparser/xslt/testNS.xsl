<xsl:transform 
 version="1.0" 
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:xslcheese = "http://cheese.org" 
 xmlns:xslbread  = "http://bread.org">

   <xsl:template match="xslbread:toast">
      xsl:template match="xslbread:toast"
      <xsl:apply-templates select="@*" />
      <xsl:apply-templates select="*" />
   </xsl:template>

   <xsl:template match="xslcheeseX:camembert" xmlns:xslcheeseX="http://cheese.org">
      xsl:template match="xslcheeseX:camembert" xmlns:xslcheeseX="http://cheese.org"
      <xsl:apply-templates select="@*" />
      <xsl:apply-templates select="*" />
   </xsl:template>

   <xsl:template match="@xslcheese:*">
      xsl:template match="@xslcheese:*"
   </xsl:template>



   <xsl:template match="xslcheese:*">
      xsl:template match="xslcheese:*"
      <xsl:apply-templates select="@*" />
      <xsl:apply-templates select="*" />
   </xsl:template>

   <xsl:template match="*" xml:space="preserve">
   <result>
      xsl:template match="*"
      <xsl:apply-templates select="@*" />
      <xsl:apply-templates select="*" />
      <xsl:element name="no-ns-name" />
      <xsl:element name="xslcheese:ns-name" xmlns:useless="useless.uri"/>
      <xsl:element name="cheese:ns-name" xmlns:cheese="http://cheese.org" />
      <xsl:element name="newns:ns-name" xmlns:newns="http://latest.org" />
      <xsl:element name="computed:ns-name" namespace="http://computed_{1+1-1}.org" />


   </result>
   </xsl:template>

</xsl:transform>