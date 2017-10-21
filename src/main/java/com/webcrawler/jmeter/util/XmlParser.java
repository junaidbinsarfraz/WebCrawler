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
import org.xml.sax.InputSource;

import com.webcrawler.util.Constants;

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
	
	// On Authentication
	public static String parseRequestArgumentXmlAndUpdateValues(String xml, Map<String, String> values, String username, String password) {
		
		xml = "<HTTPSamplerProxy>" + xml + "</HTTPSamplerProxy>"; // To make it well formed xml
		
		try {

			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(new InputSource(new ByteArrayInputStream(xml.getBytes("utf-8"))));

			XPathFactory xPathfactory = XPathFactory.newInstance();
			XPath xpath = xPathfactory.newXPath();
			
			for(String key : values.keySet()) {
				
				XPathExpression expr = xpath.compile("//elementProp[@elementType=\"HTTPArgument\" and @name=\"" + key + "\"]//stringProp[@name=\"Argument.value\"]");
				
				NodeList argumentNodes = (NodeList) expr.evaluate(doc, XPathConstants.NODESET);
				
				for(int i = 0; i < argumentNodes.getLength(); i++) {
					
					Node argumentValueNode = argumentNodes.item(i);
					
					if(argumentValueNode != null) {
						if(username.equals(argumentValueNode.getTextContent())) {
							argumentValueNode.setTextContent(Constants.USERNAME_NICKNAME);
						} else if(password.equals(argumentValueNode.getTextContent())) {
							argumentValueNode.setTextContent(Constants.PASSWORD_NICKNAME);
						} else {
							argumentValueNode.setTextContent(values.get(key));
						}
						
//						argumentValueNode.setTextContent(values.get(key));
					}
				}
			}
			
//			XPathExpression exprUsername = xpath.compile("//stringProp[@name=\"Argument.value\" and text()=\"" + username + "\"]");
//			
//			XPathExpression exprPassowrd = xpath.compile("//stringProp[@name=\"Argument.value\" and text()=\"" + password + "\"]");
//			
//			NodeList argumentNodes = (NodeList) exprUsername.evaluate(doc, XPathConstants.NODESET);
//			
//			Node argumentValueNode = argumentNodes.item(0);
//			
//			if(argumentValueNode != null) {
//				argumentValueNode.setTextContent(Constants.USERNAME_NICKNAME);
//			}
//			
//			argumentNodes = (NodeList) exprPassowrd.evaluate(doc, XPathConstants.NODESET);
//			
//			argumentValueNode = argumentNodes.item(0);
//			
//			if(argumentValueNode != null) {
//				argumentValueNode.setTextContent(Constants.PASSWORD_NICKNAME);
//			}
			
			TransformerFactory tf = TransformerFactory.newInstance();
			Transformer transformer = tf.newTransformer();
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			
			StringWriter writer = new StringWriter();
			transformer.transform(new DOMSource(doc), new StreamResult(writer));

			String updatedXml = writer.getBuffer().toString();

			updatedXml = updatedXml.replaceFirst("<HTTPSamplerProxy>", "");
			
			updatedXml = replaceLast(updatedXml , "</HTTPSamplerProxy>", "");
			
			return updatedXml;
			
		} catch (Exception e) {
			return null;
		}
	}
	
	// After Authentication
	public static String parseRequestHeaderXmlAndUpdateValues(String xml, Map<String, String> values) {
		
		xml = "<HTTPSamplerProxy>" + xml + "</HTTPSamplerProxy>"; // To make it well formed xml
		
		try {

			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(new InputSource(new ByteArrayInputStream(xml.getBytes("utf-8"))));

			XPathFactory xPathfactory = XPathFactory.newInstance();
			XPath xpath = xPathfactory.newXPath();
			XPathExpression expr = xpath.compile("//elementProp[@elementType=\"HeaderManager\"]");

			NodeList nl = (NodeList) expr.evaluate(doc, XPathConstants.NODESET);

			Node headerManagerNode = nl.item(0);

			expr = xpath.compile("//collectionProp[@name=\"HeaderManager.headers\"]");

			NodeList headerNodes = (NodeList) expr.evaluate(headerManagerNode, XPathConstants.NODESET);

			Map<String, String> attrMap = new LinkedHashMap<>();

			Node collectionPropNode = headerNodes.item(0);

			for (String key : values.keySet()) {
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

				Node stringPropHeaderNameNode = createTextNodeWithAttributes(doc, "stringProp", key, attrMap);

				elementPropNode.appendChild(stringPropHeaderNameNode);

				attrMap = new LinkedHashMap<>();

				attrMap.put("name", "Header.value");

				Node stringPropHeaderValueNode = createTextNodeWithAttributes(doc, "stringProp", values.get(key),
						attrMap);

				elementPropNode.appendChild(stringPropHeaderValueNode);

				collectionPropNode.appendChild(elementPropNode);
			}

			TransformerFactory tf = TransformerFactory.newInstance();
			Transformer transformer = tf.newTransformer();
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			
			StringWriter writer = new StringWriter();
			transformer.transform(new DOMSource(doc), new StreamResult(writer));
			
			String updatedXml = writer.getBuffer().toString();

			updatedXml = updatedXml.replaceFirst("<HTTPSamplerProxy>", "");
			
			updatedXml = replaceLast(updatedXml , "</HTTPSamplerProxy>", "");

			return updatedXml;

		} catch (Exception e) {
			return null;
		}
	}

	private static Node createTextNodeWithAttributes(Document doc, String tagName, String tagValue,
			Map<String, String> attrMap) {

		Node node = null;

		if (tagName != null && tagName != "") {
			node = doc.createElement(tagName);

			node.setTextContent(tagValue);

			for (String key : attrMap.keySet()) {

				Attr attr = doc.createAttribute(key);

				attr.setNodeValue(attrMap.get(key));

				node.getAttributes().setNamedItem(attr);
			}
		}

		return node;
	}
	
	private static String replaceLast(String text, String regex, String replacement) {
        return text.replaceFirst("(?s)(.*)" + regex, "$1" + replacement);
    }

}
