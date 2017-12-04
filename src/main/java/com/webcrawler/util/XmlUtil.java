package com.webcrawler.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * The class XmlUtil is use to provide xml utilities
 * 
 * @author Junaid
 */
public class XmlUtil {
	
	/**
	 * The method buildXml() is use to build xml from given string
	 * 
	 * @param xml
	 *            to be built
	 * @return Built Xml
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws IOException
	 * @throws UnsupportedEncodingException
	 */
	public static Document buildXml(String xml)
			throws ParserConfigurationException, SAXException, IOException, UnsupportedEncodingException {
		
		DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
		Document doc = docBuilder.parse(new InputSource(new ByteArrayInputStream(xml.replaceAll("&", "&amp;").getBytes("utf-8"))));
		
		return doc;
	}
	
	/**
	 * The method transformXml() is use to transform xml to string format
	 * 
	 * @param doc
	 *            to be transformed
	 * @return transformed xml
	 * @throws TransformerFactoryConfigurationError
	 * @throws TransformerConfigurationException
	 * @throws TransformerException
	 */
	public static String transformXml(Document doc)
			throws TransformerFactoryConfigurationError, TransformerConfigurationException, TransformerException {
		
		TransformerFactory tf = TransformerFactory.newInstance();
		Transformer transformer = tf.newTransformer();
		transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		
		StringWriter writer = new StringWriter();
		transformer.transform(new DOMSource(doc), new StreamResult(writer));

		String updatedXml = writer.getBuffer().toString().replaceAll("&amp;", "&");
		return updatedXml;
	}
	
}
