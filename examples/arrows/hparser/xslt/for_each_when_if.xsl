<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <result>
  <xsl:for-each select="*/element_type1">
      Nr:<xsl:value-of select="@nr"/>:

      with if:

      <xsl:if test="@nr>3"> Number is larger than 3
      </xsl:if>

      <xsl:if test="@nr>2"> Number is larger than 2
      </xsl:if>

      <xsl:if test="@nr>1"> Number is larger than 1
      </xsl:if>

      <xsl:if test="@nr>0"> Number is larger than 0
      </xsl:if>

      with choose:
      
      <xsl:choose>

      <xsl:when test="@nr>3"> Number is larger than 3
      </xsl:when>

      <xsl:when test="@nr>2"> Number is larger than 2
      </xsl:when>

      <xsl:when test="@nr>1"> Number is larger than 1
      </xsl:when>

      <xsl:when test="@nr>0"> Number is larger than 0
      </xsl:when>

      <xsl:otherwise> Number is smaller than 1
      </xsl:otherwise>

      </xsl:choose>

  </xsl:for-each>
  </result>
</xsl:template>

</xsl:transform>