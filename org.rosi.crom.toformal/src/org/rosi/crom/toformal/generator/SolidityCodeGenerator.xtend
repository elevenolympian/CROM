package org.rosi.crom.toformal.generator

import crom_l1_composed.Model
import generator.CROMGenerator
import org.eclipse.swt.widgets.Shell
import org.eclipse.core.runtime.IPath
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.jface.dialogs.MessageDialog


class SolidityCodeGenerator extends AbstractCROMGenerator{
	
	public new() {
		super("solidity");
	}
//	def static void main(String[] args) {
//		
//	}

	//Dummy Generator for Xtend
	def generateDummy(Shell shell, IPath path, Resource resource) {
		var s=""
		for (o: resource.contents){
			val m=o as Model
			for (e:m.elements){
				s+=e.getName()+"\n"
			}
		}		
		val transformation = s;				
		val n=path.removeFileExtension().addFileExtension("txt");
		MessageDialog.openInformation(shell,"Simulated Generation for Solidity that has: "+n,transformation);			
	}	
	
	
	override generate(String modelName, Model model) 
	{
		var transformation = "";
		try {
			System.out.println("Modelname is: " + modelName)
			transformation = new CROMGenerator(true).generate(model)
			if(!modelName.equalsIgnoreCase(null))
				transformation = transformation.replace("CROMApplication",modelName)
		} catch(Exception ex)
		{
			throw new UnsupportedOperationException("TODO: auto-generated method stub")
		}
		return transformation;
	}
	
		private val mapping=newHashMap(  //Solidity data structure mapping
		"String" -> "VARCHAR",
		"bool" -> "BOOLEAN", "Boolean" -> "BOOLEAN",
		"int" -> "INT", "Integer" -> "INT",
		"Double" -> "DOUBLE", "double" -> "DOUBLE",
		"float" -> "FLOAT", "Float" -> "FLOAT",
		"Time" -> "TIME",
		"Date" -> "DATE",
		"DateTime" -> "DATETIME")
	
	
}