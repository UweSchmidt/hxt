<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:text="http://text.org">

<xsl:strip-space elements="*" />
<xsl:preserve-space elements="text:*" />

<xsl:template match="/">
  <xsl:copy-of select="/" />
</xsl:template>

</xsl:transform>
