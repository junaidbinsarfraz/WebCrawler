package com.webcrawler.jmeter.util;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.commons.io.FileUtils;
import org.w3c.dom.Document;

/**
 * The class XmlParser is use to parse the xml to get desire data or sub-xml
 * 
 * @author Junaid
 */
public class XmlParser {

	/**
	 * The method parseXmlWithGivenXPathExp() method is use to parse xml with
	 * give XPath expression
	 * 
	 * @param xmlFile
	 *            contains the XML
	 * @param xpathExp
	 *            XPATH expression
	 * @return parsed xml
	 */
	public static String parseXmlWithGivenXPathExp(File xmlFile, String xpathExp) {

		String parsedXml = "";

		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();

			ByteArrayInputStream byteArray = new ByteArrayInputStream(FileUtils.readFileToByteArray(xmlFile));

			Document doc = builder.parse(byteArray);

			// XPath
			/*
			 * XPath xpath = XPathFactory.newInstance().newXPath();
			 * XPathExpression expr = xpath.compile(xpathExp); Object exprResult
			 * = expr.evaluate(doc, XPathConstants.NODESET); NodeList nodeList =
			 * (NodeList) exprResult;
			 * 
			 * System.out.println(nodeList.getLength());
			 * System.out.println(nodeList.toString());
			 */

			// TODO: XSLT
			/*
			 * TransformerFactory transformerFactory =
			 * TransformerFactory.newInstance(); Source xslt = new
			 * StreamSource(new File("httpsamplerproxy-transformer.xslt"));
			 * Transformer transformer =
			 * transformerFactory.newTransformer(xslt);
			 * 
			 * Source text = new StreamSource(xmlFile);
			 * transformer.transform(text, new StreamResult(arg0));
			 */
		} catch (Exception e) {
			return null;
		}

		return parsedXml;
	}

	/**
	 * The method parseXmlWithXslTransformer() is use to transform give xml with
	 * the XSL expression
	 * 
	 * @param jmxHashTree
	 *            to be transformed
	 * @param br
	 *            input XSL expression
	 * @return transformed xml
	 */
	public static String parseXmlWithXslTransformer(String jmxHashTree, BufferedReader br) {

		StreamResult streamResult = new StreamResult();

		try {
			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			Source xslt = new StreamSource(br);
			Transformer transformer = transformerFactory.newTransformer(xslt);
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");

			OutputStream out = new ByteArrayOutputStream();

			streamResult.setOutputStream(out);

			Source text = new StreamSource((InputStream) (new ByteArrayInputStream(jmxHashTree.getBytes())));
			transformer.transform(text, streamResult);
		} catch (Exception e) {
			return null;
		}

		return streamResult.getOutputStream().toString();
	}

}
