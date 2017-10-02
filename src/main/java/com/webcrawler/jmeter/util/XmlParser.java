package com.webcrawler.jmeter.util;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import org.apache.commons.io.FileUtils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.webcrawler.util.Util;

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
	 * @param xml
	 *            to be transformed
	 * @param br
	 *            input XSL expression
	 * @return transformed xml
	 */
	public static String parseXmlWithXslTransformer(String xml, BufferedReader br) {

		StreamResult streamResult = new StreamResult();

		try {
			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			Source xslt = new StreamSource(br);
			Transformer transformer = transformerFactory.newTransformer(xslt);
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");

			OutputStream out = new ByteArrayOutputStream();

			streamResult.setOutputStream(out);

			Source text = new StreamSource((InputStream) (new ByteArrayInputStream(xml.getBytes())));
			transformer.transform(text, streamResult);
		} catch (Exception e) {
			return null;
		}

		return streamResult.getOutputStream().toString();
	}
	
	public static String parseRequestHeaderXmlAndUpdateValues(String xml, Map<String, String> values) {
		
		try {
			
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(xml);
			
			// Get the root element
//			Node httpSamplerProxy = doc.getFirstChild();
			
			XPathFactory xPathfactory = XPathFactory.newInstance();
			XPath xpath = xPathfactory.newXPath();
			XPathExpression expr = xpath.compile("//elementProp[@elementType=\"HeaderManager\"]");
			
			NodeList nl = (NodeList) expr.evaluate(doc, XPathConstants.NODESET);
			
			for(int i = 0; i < nl.getLength(); i++) {
				Node headerManagerNode = nl.item(i);
				
				expr = xpath.compile("//collectionProp[@name=\"HeaderManager.headers\"]");
				
				NodeList headerNodes = (NodeList) expr.evaluate(headerManagerNode, XPathConstants.NODESET);
				
				Map<String, String> attrMap = new LinkedHashMap<>();
				
				for(int j = 0; j < headerNodes.getLength(); i++) {
					
					Node collectionPropNode = headerNodes.item(j);
					
					for(String key : values.keySet()) {
						// Make new elementProp
						Node elementPropNode = doc.createElement("elementProp");
						
						Attr elementPropNodeNameAttr = doc.createAttribute("name");
						
						elementPropNodeNameAttr.setNodeValue(key);
						
						elementPropNode.getAttributes().setNamedItem(elementPropNodeNameAttr);
						
						Attr elementPropNodeElementTypeAttr = doc.createAttribute("elementType");

						elementPropNodeElementTypeAttr.setNodeValue("Header");
						
						elementPropNode.getAttributes().setNamedItem(elementPropNodeElementTypeAttr);
						
						attrMap = new LinkedHashMap<>();
						
						attrMap.put("name", "Header.name");
						
						Node stringPropHeaderNameNode = createTextNodeWithAttributes(doc, "stringProp", values.get(key), attrMap);
						
						elementPropNode.appendChild(stringPropHeaderNameNode);
							
						//
						attrMap = new LinkedHashMap<>();
						
						attrMap.put("name", "Header.value");
						
						Node stringPropHeaderValueNode = createTextNodeWithAttributes(doc, "stringProp", values.get(key), attrMap);
						
						elementPropNode.appendChild(stringPropHeaderValueNode);
						
						
						
						
						collectionPropNode.appendChild(elementPropNode);
					}
				}
			}
			
			TransformerFactory tf = TransformerFactory.newInstance();
			Transformer transformer = tf.newTransformer();
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
			StringWriter writer = new StringWriter();
			transformer.transform(new DOMSource(doc), new StreamResult(writer));
			
			return writer.getBuffer().toString();
			
		} catch (Exception e) {
			return null;
		}
	}
	
	private static Node createTextNodeWithAttributes(Document doc, String tagName, String tagValue, Map<String, String> attrMap) {
		
		Node node = null;
		
		if(Util.isNotNullAndEmpty(tagName)) {
			node = doc.createElement(tagName);
			
			node.setNodeValue(tagValue);
			
			for(String key : attrMap.keySet()) {
				
				Attr attr = doc.createAttribute(key);
				
				attr.setNodeValue(attrMap.get(key));
				
				node.getAttributes().setNamedItem(attr);
			}
		}
		
		return node;
	}

}
