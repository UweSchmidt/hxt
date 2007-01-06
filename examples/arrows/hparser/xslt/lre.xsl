<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <result attr="value">
      <nested_1 attr_nested="value is {{{1+1}}} ({1+1}) :-}} :-{{)">
         textnode
      </nested_1>
  </result>
</xsl:template>

</xsl:transform>