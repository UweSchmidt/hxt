<xsl:transform version="1.0" 
  xml:strip-space="preserve"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:extension="extension.org" 
  extension-element-prefixes="extension"
  xmlns:ns1="ns1.org">

  <xsl:template match="*">
    <ns1:result/>
  </xsl:template>

  <xsl:template name="procedure"
      exclude-result-prefixes="ns1"
      xmlns:ns2="ns2.org">
    <ns2:result/>
  </xsl:template>

</xsl:transform>