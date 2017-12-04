package com.webcrawler.parser;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.webcrawler.util.Constants;
import com.webcrawler.util.Util;
import com.webcrawler.util.XmlUtil;

/**
 * The class XmlParser is use to parse the xml to get desire data or sub-xml
 * 
 * @author Junaid
 */
public class XmlParser {

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

			Source text = new StreamSource((InputStream) (new ByteArrayInputStream(xml.replaceAll("&", "&amp;").getBytes())));
			transformer.transform(text, streamResult);
		} catch (Exception e) {
			return null;
		}

		return streamResult.getOutputStream().toString().replaceAll("&amp;", "&");
	}
	
	// On Authentication
	/**
	 * The method parseRequestArgumentXmlAndUpdateValues() is use to parse xml
	 * and replace keys' values (change from old value with new one), username
	 * and password
	 * 
	 * @param xml
	 *            JMX to be parsed
	 * @param values
	 *            Map contains keys and values
	 * @param username
	 *            to be changed with USERNAME_NICKNAME
	 * @param password
	 *            to be changed with PASSWORD_NICKNAME
	 * @return xml string
	 */
	public static String parseRequestArgumentXmlAndUpdateValues(String xml, Map<String, String> values, String username, String password) {
		
		try {
			xml = "<HTTPSamplerProxy>" + xml + "</HTTPSamplerProxy>"; // To make it well formed xml

			Document doc = XmlUtil.buildXml(xml);

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
					}
				}
			}
			
			String updatedXml = XmlUtil.transformXml(doc);

			updatedXml = updatedXml.replaceFirst("<HTTPSamplerProxy>", "");
			
			updatedXml = Util.replaceLast(updatedXml , "</HTTPSamplerProxy>", "");
			
			return updatedXml;
			
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * The method addRequestParametersAsRegexExtractors() is use to append
	 * regexExtrators at the end of xml
	 * 
	 * @param xml
	 *            where regexExtractors will be appended
	 * @param regexExtractors
	 *            Map containing keys values to be appended
	 * @return xml appended with regexExtractors
	 */
	public static String addRequestParametersAsRegexExtractors(String xml, List<Node> regexExtractors) {
		try {
			
			xml = "<HTTPSamplerProxy>" + xml + "</HTTPSamplerProxy>"; // To make it well formed xml

			Document doc = XmlUtil.buildXml(xml);

			appendExtractors(regexExtractors, doc);
			
			String updatedXml = XmlUtil.transformXml(doc);

			updatedXml = updatedXml.replaceFirst("<HTTPSamplerProxy>", "");
			
			updatedXml = Util.replaceLast(updatedXml , "</HTTPSamplerProxy>", "");
			
			return updatedXml;
			
		} catch (Exception e) {
			return null;
		}
	}

	// After Authentication
	/**
	 * The method parseRequestHeaderXmlAndUpdateValues() is use to parse xml,
	 * update request headers with values and append regexExtractors
	 * 
	 * @param xml
	 *            to be parsed
	 * @param values
	 *            Map containing keys values to be changed
	 * @param regexExtractors
	 *            To be appended
	 * @return updated xml
	 */
	public static String parseRequestHeaderXmlAndUpdateValues(String xml, Map<String, String> values, List<Node> regexExtractors) {
		
		xml = "<HTTPSamplerProxy>" + xml + "</HTTPSamplerProxy>"; // To make it well formed xml
		
		try {

			Document doc = XmlUtil.buildXml(xml);

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
			
			String updatedXml = XmlUtil.transformXml(doc);

			updatedXml = updatedXml.replaceFirst("<HTTPSamplerProxy>", "");
			
			updatedXml = Util.replaceLast(updatedXml , "</HTTPSamplerProxy>", "");

			return updatedXml;

		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * The method appendExtractors() is use to append regexExtractors at the end
	 * of doc
	 * 
	 * @param regexExtractors
	 *            to be appended
	 * @param doc
	 *            to be parsed
	 */
	private static void appendExtractors(List<Node> regexExtractors, Document doc) {
		Node dummyHTTPSamplerProxyNode = doc.getFirstChild();
		
		Node hashTreeNode = doc.createElement("hashTree");
		
		for(Node regexExtractor : regexExtractors) {
			
			Node regexExtractorImportedNode = doc.importNode(regexExtractor, Boolean.TRUE);
			
			dummyHTTPSamplerProxyNode.appendChild(regexExtractorImportedNode);
			
			hashTreeNode = doc.createElement("hashTree");
			
			dummyHTTPSamplerProxyNode.appendChild(hashTreeNode);
		}
	}
	
	/**
	 * The method createRegexExtractors() is use to create regexExtractors from
	 * given corrRegexAndVariables
	 * 
	 * @param corrRegexAndVariables
	 *            to be used to create regexExtractors
	 * @param isRequestHeaderValues
	 *            true while creating corrRegexExtractors from request headers
	 * @return list of regex extractors
	 */
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
	
	/**
	 * The method createRegexExtractor() is use to create one regexExtractor
	 * Node with given regex, refname and isRequestHeaderValues
	 * 
	 * @param regex
	 *            regex
	 * @param refname
	 *            reference name
	 * @param isRequestHeaderValues
	 *            true while creating corrRegexExtractors from request headers
	 * @return regex extractors
	 */
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

	/**
	 * The method createTextNodeWithAttributes() is use to create text node with
	 * attribute
	 * 
	 * @param doc
	 *            to be parsed
	 * @param tagName
	 *            tag name
	 * @param tagValue
	 *            tag value
	 * @param attrMap
	 *            attribute map
	 * @return text node
	 */
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
	
}
