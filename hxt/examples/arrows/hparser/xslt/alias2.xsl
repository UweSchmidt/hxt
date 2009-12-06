<xsl:transform version    = "1.0" 
               xmlns:xsl  = "http://www.w3.org/1999/XSL/Transform"
               xmlns:axsl = "http://xsldude.org" >  

  <xsl:namespace-alias stylesheet-prefix="axsl" result-prefix="xsl" />

  <xsl:template match="/" >
    <lre-stylesheet axsl:version="1.0" />
  </xsl:template>

</xsl:transform>