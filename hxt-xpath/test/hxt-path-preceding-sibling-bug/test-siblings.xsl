<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="top">
      <xsl:value-of select="third/preceding-sibling::*[1]"/>
      <xsl:value-of select="third/preceding-sibling::*[last()]"/>
      <xsl:value-of select="third/following-sibling::*[1]"/>
      <xsl:value-of select="third/following-sibling::*[last()]"/>
  </xsl:template>
</xsl:stylesheet>
