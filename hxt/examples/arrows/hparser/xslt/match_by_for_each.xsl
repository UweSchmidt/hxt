<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="document">
    <xsl:for-each select="*/*">
      <xsl:choose>
        <xsl:when test="self::b[parent::a[not (parent::*)]]">
          Match: a/b   
        </xsl:when>
        <xsl:when test="self::b">
          Match: b
        </xsl:when>
        <xsl:when test="self::*">
          Match: *
        </xsl:when>
        <xsl:otherwise>
          Match: the rest
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
</xsl:transform>