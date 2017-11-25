package com.webcrawler.jmeter.util;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
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
import org.xml.sax.SAXException;

import com.webcrawler.dao.RequestCorrelationTbl;
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
		
		try {
			xml = "<HTTPSamplerProxy>" + xml + "</HTTPSamplerProxy>"; // To make it well formed xml

			Document doc = buildXml(xml);

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
			
			String updatedXml = transformXml(doc);

			updatedXml = updatedXml.replaceFirst("<HTTPSamplerProxy>", "");
			
			updatedXml = replaceLast(updatedXml , "</HTTPSamplerProxy>", "");
			
			return updatedXml;
			
		} catch (Exception e) {
			return null;
		}
	}

	private static Document buildXml(String xml)
			throws ParserConfigurationException, SAXException, IOException, UnsupportedEncodingException {
		
		DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
		Document doc = docBuilder.parse(new InputSource(new ByteArrayInputStream(xml.getBytes("utf-8"))));
		
		return doc;
	}
	
	public static String addRequestParametersAsRegexExtractors(String xml, List<Node> regexExtractors) {
		try {
			
			xml = "<HTTPSamplerProxy>" + xml + "</HTTPSamplerProxy>"; // To make it well formed xml

			Document doc = buildXml(xml);

			appendExtractors(regexExtractors, doc);
			
			String updatedXml = transformXml(doc);

			updatedXml = updatedXml.replaceFirst("<HTTPSamplerProxy>", "");
			
			updatedXml = replaceLast(updatedXml , "</HTTPSamplerProxy>", "");
			
			return updatedXml;
			
		} catch (Exception e) {
			return null;
		}
	}

	private static String transformXml(Document doc)
			throws TransformerFactoryConfigurationError, TransformerConfigurationException, TransformerException {
		
		TransformerFactory tf = TransformerFactory.newInstance();
		Transformer transformer = tf.newTransformer();
		transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		
		StringWriter writer = new StringWriter();
		transformer.transform(new DOMSource(doc), new StreamResult(writer));

		String updatedXml = writer.getBuffer().toString();
		return updatedXml;
	}
	
	// After Authentication
	public static String parseRequestHeaderXmlAndUpdateValues(String xml, Map<String, String> values, List<Node> regexExtractors) {
		
		xml = "<HTTPSamplerProxy>" + xml + "</HTTPSamplerProxy>"; // To make it well formed xml
		
		try {

			Document doc = buildXml(xml);

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
			
			appendExtractors(regexExtractors, doc);
			
			String updatedXml = transformXml(doc);

			updatedXml = updatedXml.replaceFirst("<HTTPSamplerProxy>", "");
			
			updatedXml = replaceLast(updatedXml , "</HTTPSamplerProxy>", "");

			return updatedXml;

		} catch (Exception e) {
			return null;
		}
	}

	private static void appendExtractors(List<Node> regexExtractors, Document doc) {
		Node dummyHTTPSamplerProxyNode = doc.getFirstChild();
		
		Node hashTreeNode = doc.createElement("hashTree");
		
//			dummyHTTPSamplerProxyNode.appendChild(hashTreeNode);
		
		for(Node regexExtractor : regexExtractors) {
			
			Node regexExtractorImportedNode = doc.importNode(regexExtractor, Boolean.TRUE);
			
			dummyHTTPSamplerProxyNode.appendChild(regexExtractorImportedNode);
			
			hashTreeNode = doc.createElement("hashTree");
			
			dummyHTTPSamplerProxyNode.appendChild(hashTreeNode);
		}
	}
	
	public static List<Node> createRegexExtractors(Map<String, String> corrRegexAndVariables, Boolean isRequestHeaderValues) {
		List<Node> regexExtractors = new ArrayList<>();
		
		try {
			
			// TODO: Remove these 3 unsed lines
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.newDocument();
			
			for(String key : corrRegexAndVariables.keySet()) {
				String value = corrRegexAndVariables.get(key);
				
				regexExtractors.add(createRegexExtractor(key, value, isRequestHeaderValues));
			}
			
		} catch (Exception e) {
			
		}
		
		return regexExtractors;
	}
	
	private static Node createRegexExtractor(String regex, String refname, Boolean isRequestHeaderValues) {
		
		Node regexExtractorNode = null;
		
		try {
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.newDocument();
			
			Map<String, String> attrMap = new HashMap<>();
			
			attrMap.put("name", "RegexExtractor.useHeaders");
			
			Node userHeadersNode = createTextNodeWithAttributes(doc, "stringProp", Boolean.TRUE.equals(isRequestHeaderValues) ? "true" : "false", attrMap);
			
			attrMap = new HashMap<>();
			
			attrMap.put("name", "RegexExtractor.refname");
			
			refname = refname.replace("$", "");
			refname = refname.replace("{", "");
			refname = refname.replace("}", "");
			
			Node rfnameNode = createTextNodeWithAttributes(doc, "stringProp", refname, attrMap);
			
			attrMap = new HashMap<>();
			
			attrMap.put("name", "RegexExtractor.regex");
			
			Node regexNode = createTextNodeWithAttributes(doc, "stringProp", regex, attrMap);
			
			attrMap = new HashMap<>();
			
			attrMap.put("name", "RegexExtractor.template");
			
			Node templateNode = createTextNodeWithAttributes(doc, "stringProp", "$1$", attrMap);
			
			attrMap = new HashMap<>();
			
			attrMap.put("name", "RegexExtractor.default");
			
			Node defaultNode = createTextNodeWithAttributes(doc, "stringProp", Boolean.TRUE.equals(isRequestHeaderValues) ? "1" : "notfound", attrMap);
			
			attrMap = new HashMap<>();
			
			attrMap.put("name", "RegexExtractor.match_number");
			
			Node matchNumberNode = createTextNodeWithAttributes(doc, "stringProp", "1", attrMap);
			
			attrMap = new HashMap<>();
			
			attrMap.put("name", "Scope.variable");
			
			Node variableNode = createTextNodeWithAttributes(doc, "stringProp", Boolean.TRUE.equals(isRequestHeaderValues) ? "" : "all", attrMap);
			
			regexExtractorNode = doc.createElement("RegexExtractor");
			
			Attr guiclassAttr = doc.createAttribute("guiclass");

			guiclassAttr.setNodeValue("RegexExtractorGui");
			
			Attr testclassAttr = doc.createAttribute("testclass");

			testclassAttr.setNodeValue("RegexExtractor");
			
			Attr testnameAttr = doc.createAttribute("testname");

			testnameAttr.setNodeValue("Regular Expression Extractor");
			
			Attr enabledAttr = doc.createAttribute("enabled");

			enabledAttr.setNodeValue("true");

			regexExtractorNode.getAttributes().setNamedItem(guiclassAttr);
			regexExtractorNode.getAttributes().setNamedItem(testclassAttr);
			regexExtractorNode.getAttributes().setNamedItem(testnameAttr);
			regexExtractorNode.getAttributes().setNamedItem(enabledAttr);
			
			regexExtractorNode.appendChild(userHeadersNode);
			regexExtractorNode.appendChild(rfnameNode);
			regexExtractorNode.appendChild(regexNode);
			regexExtractorNode.appendChild(templateNode);
			regexExtractorNode.appendChild(defaultNode);
			regexExtractorNode.appendChild(matchNumberNode);
			regexExtractorNode.appendChild(variableNode);
			
		} catch (Exception e) {
			return null;
		}
		
		return regexExtractorNode;
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
