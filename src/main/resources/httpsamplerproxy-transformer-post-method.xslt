<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	version="1.0">
	<xsl:output method="xml" />
	<xsl:variable name="doc-file">
		http://mymachine.com/changed.xml
	</xsl:variable>
	<xsl:template match="/">
		<xsl:copy-of select="(//HTTPSamplerProxy)[last()]" />
	</xsl:template>
</xsl:stylesheet>