<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- Import-file for var_and_params_imp.xsl -->

       <xsl:template match="document">
           <xsl:param name="P1" select="'imported P1'" />
           <xsl:param name="P2" select="'imported P2'" />
           <xsl:param name="P3" select="'imported P3'" />
           <xsl:param name="P4" select="'imported P4'" />

           <template_match_document_imported>           
 
               Value of variable A:<xsl:value-of select="$A"/>:
               Value of variable B:<xsl:value-of select="$B"/>:
               Value of variable C:<xsl:value-of select="$C"/>:

               Value of parameter P1:<xsl:value-of select="$P1"/>:
               Value of parameter P2:<xsl:value-of select="$P2"/>:
               Value of parameter p3:<xsl:value-of select="$P3"/>:
               Value of parameter p3:<xsl:value-of select="$P4"/>:

               <xsl:call-template name="function">
                  <xsl:with-param name="depth" select="5"/>
               </xsl:call-template>

           </template_match_document_imported>
       </xsl:template>



</xsl:transform>
