<xsl:transform version="1.0" 
  xml:space="preserve"
  xmlns:unused="unused.org"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:extension="extension.org" 
  extension-element-prefixes="extension"
  xmlns:ns1="ns1.org">

  <xsl:template match="*" xml:space="preserve"
>
    <xsl:element name="result">
      <ns1:result/>
      <xsl:call-template name="procedure" />
    </xsl:element>
  </xsl:template>

  <xsl:template name="procedure" xmlns:ns2="ns2.org">
    <ns2:result xsl:exclude-result-prefixes="ns1">
      <inner/>
    </ns2:result>
  </xsl:template>

</xsl:transform>