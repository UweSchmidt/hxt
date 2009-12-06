<xsl:transform 
 xml:space="preserve"
 version="1.0" 
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:axsl="http://xsldude.org" >
  
  <xsl:namespace-alias stylesheet-prefix="axsl" result-prefix="xsl" />   

  <xsl:template match="/">
    <axsl:transform version="1.0">
      <axsl:template match="/">
        <axsl:message>This XSL stylesheet is created from literal result elements</axsl:message>
      </axsl:template>
    </axsl:transform> 
  </xsl:template>
</xsl:transform>