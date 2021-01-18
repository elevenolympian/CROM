package org.rosi.crom.toformal.generator

import crom_l1_composed.Model
import java.text.SimpleDateFormat
import java.util.Calendar

import org.rosi.crom.toformal.builder.CROMVisitor
import org.rosi.crom.toformal.builder.CROModel
import org.rosi.crom.toformal.builder.Cardinality
import org.rosi.crom.toformal.builder.RoleGroup
import java.util.Set
import java.util.HashMap
import java.util.Map
import java.util.List
import java.util.Collections

class SolidityCodeGenerator extends AbstractCROMGenerator{
	
	val dateFormat = new SimpleDateFormat("YYYY-MM-dd")
	val cal = Calendar.getInstance()
	var int numberOfRoleGroups
	val int headingWidth = 100
	val boolean compTypesPlayRoles     = false
	val int roleGroupDeclaration       = 1
	val boolean separateRoleTypeAxioms = true
	val boolean localRSTAxioms         = true
	val int fillsAxioms                = 2
	
	val boolean useNominalsAsRTs       = true
	
	
	var CROModel crom
	var Set<String> compartmentTypes
	var Set<String> naturalTypes
	var Set<String> roleTypes
	var Set<String> relationshipTypes
	var HashMap<String, RoleGroup> rgNames
	var Set<RoleGroup> roleGroups
	
	
	
	new() {
		super("sol");
		crom = new CROModel;
	}
	
	
	
	//Dummy Generator for Xtend
//	def generateDummy(Shell shell, IPath path, Resource resource) {
//		var s=""
//		for (o: resource.contents){
//			val m=o as Model
//			for (e:m.elements){
//				s+=e.getName()+"\n"
//			}
//		}		
//		val transformation = s;				
//		val n=path.removeFileExtension().addFileExtension("txt");
//		MessageDialog.openInformation(shell,"Simulated Generation for Solidity that has: "+n,transformation);			
//	}	
	
	
//	override generate(String modelName, Model model) 
//	{
//		var transformation = "";
//		try {
//			System.out.println("Modelname is: " + modelName)
//			transformation = new CROMGenerator(true).generate(model)
//			if(!modelName.equalsIgnoreCase(null))
//				transformation = transformation.replace("CROMApplication",modelName)
//		} catch(Exception ex)
//		{
//			throw new UnsupportedOperationException("TODO: auto-generated method stub")
//		}
//		return transformation;
//	}



		public override generate(String modelname, Model model) 
		{
			crom = new CROModel
			val visitor = new CROMVisitor
			var name=modelname
			if (modelname.isEmpty)
				name = "CROMSolidity"
			visitor.visit(crom, model)
			setUpSolidity()
			return printSolidity(name)
//			if (useNominalsAsRTs) {
////				return printOntologyWithNominals(name)	
//			return printHeader(modelname)
//			} else {
////				return printOntology(name)
//			}
		}
		
		private def String printSolidity(String modelName) '''
		«debug»
		«printHeader(modelName)»
		«printNaturalType»
		'''
		
		
		private def void setUpSolidity() {
			
			numberOfRoleGroups = 0
			rgNames = new HashMap()
			
			retrieveCompartmentType
			retrieveNaturalType
			retrieveRoleType
			retrieveRelationshipTypes
			retrieveRoleGroupNames
			checkCompartmentInheritance
			checkNaturalType
			checkRelCardConnection
			checkInterRSTConstraintsEmpty
			checkIntraRSTConstraintsEmpty
	//		checkRoleType
			
		}
		
	
//		private val mapping=newHashMap(  //Solidity data structure mapping
//		"String" -> "VARCHAR",
//		"bool" -> "BOOLEAN", "Boolean" -> "BOOLEAN",
//		"int" -> "INT", "Integer" -> "INT",
//		"Double" -> "DOUBLE", "double" -> "DOUBLE",
//		"float" -> "FLOAT", "Float" -> "FLOAT",
//		"Time" -> "TIME",
//		"Date" -> "DATE",
//		"DateTime" -> "DATETIME")

	
		
	private def String repeat(String s, int n) {
		return String.join("", Collections.nCopies(n, s));
	}

	
		/** 
	 * Convert crom.ct from ArrayList<String> to Set<String>.
	 */
	private def void retrieveCompartmentType() {
		compartmentTypes = crom.ct.toSet
	}
	
	/** 
	 * Convert crom.rst from ArrayList<String> to Set<String>.
	 */
	private def void retrieveRelationshipTypes() {
		relationshipTypes = crom.rst.toSet
	}
	
	/**
	 * This method checks whether an object is a role group.
	 */
	private def Boolean isRoleGroup(Object obj) {
		return (obj.class.equals(RoleGroup))
	}
	
	
	/** 
	 * Convert crom.rt from ArrayList<String> to Set<String>.
	 */
	private def void retrieveRoleType() {
		roleTypes = crom.rt.toSet
	}
	
	/** 
	 * Convert crom.nt from ArrayList<String> to Set<String>.
	 */
	private def void retrieveNaturalType() {
		naturalTypes = crom.nt.toSet
	}
	
	private def Set<RoleGroup> getNestedRoleGroups(RoleGroup roleGroup) {
		return (roleGroup.elements
			.filter[ elem | elem.isRoleGroup ]
			.map[ elem | elem as RoleGroup ]
			.map[ elem | elem.nestedRoleGroups + #{elem} ]
			.flatten + #{roleGroup}).toSet
	}
	
	
	private def Set<RoleGroup> getTopLevelRoleGroups() {
		return occurrenceConstraintsForRoleGroups
			.mapValues[ listOfConstraints | listOfConstraints.map[ entry | entry.value ].toSet ]
			.entrySet
			.map[ entry | entry.value ]
			.flatten
			.toSet
	}	
	
	/**
	 * This method retrieves these occurrence constraints that talk about role groups.
	 */
	private def Map<String,List<Pair<Cardinality,RoleGroup>>> getOccurrenceConstraintsForRoleGroups() {
		return crom.rolec.filter[ compType, listOfConstraints | listOfConstraints.map[ e | e.value.isRoleGroup].contains(true) ]
			.mapValues[ listOfConstraints | listOfConstraints
				.filter[ e | e.value.isRoleGroup ]
				.map[ e | e.key -> e.value as RoleGroup ]
				.toList]
	}
	
	
	
	/**
	 * This method retrieves all role groups that appear in the CROM.
	 */
	private def Set<RoleGroup> getAllRoleGroups() {
		topLevelRoleGroups.map[ roleGroup | roleGroup.nestedRoleGroups ].flatten.toSet
	}
	
		/**
	 * This method generates a fresh generic name for a role group and increases the counter of
	 * created role group names by 1.
	 */
	private def String createNewRoleGroupName() {
		numberOfRoleGroups++
		return "GeneratedRoleGroup" + numberOfRoleGroups
	}
	
	
	
	/**
	 * This method constructs the HashMap that identifies role groups by their (generated) names.
	 */
	private def void retrieveRoleGroupNames() {
		allRoleGroups.forEach[ roleGroup | rgNames.put(createNewRoleGroupName, roleGroup)]
		roleGroups = rgNames.values.toSet
	}
		/**
	 * This method checks whether in the given CROM an inheritance relation between compartment
	 * types is present, and if so throws an exception. 
	 */
	private def void checkCompartmentInheritance() {
		if (!crom.ctinh.empty)
			throw new CROMOntologyGeneratorException(
				"Compartment inheritance is not supported by the ontology generator!\n"
				+ crom.ctinh.join("\n"))
	}
	
	/**
	 * This method checks whether in the given CROM all names of natural types are distinct, and if
	 * not so throws an exception. 
	 */
	private def void checkNaturalType() {
		if (crom.nt.length != naturalTypes.length)
			throw new CROMOntologyGeneratorException(
				"Multiple natural types with identical names are detected!\n"
				+ "Names must be unique!")
	}

	/**
	 * This method checks whether the key sets of the cardinal constraints and the relationship
	 * domain and range constraints are equal.
	 */
	private def void checkRelCardConnection() {
		if (!crom.rel.keySet.equals(crom.card.keySet))
			throw new CROMOntologyGeneratorException(
				"The key sets of the cardinal constraints and relationship domain and range constraints must be the same!"
			)
	}
	
	/**
	 * This method checks whether there are no inter relationship type constraints, and if so
	 * throws an exception since the ontology generator does not support these kind of constraints.
	 */
	 private def void checkInterRSTConstraintsEmpty() {
	 	if (!crom.inter.empty)
	 		throw new CROMOntologyGeneratorException(
	 			"Inter relationship type constraints, like relationship type implications, are not supported!"
	 		)
	 }
	
	/**
	 * This method checks whether there are no intra relationship type constraints, and if so
	 * throws an exception since the ontology generator does not support these kind of constraints.
	 */
	 private def void checkIntraRSTConstraintsEmpty() {
	 	if (!crom.intra.empty)
	 		throw new CROMOntologyGeneratorException(
	 			"Intra relationship type constraints, like irreflexive relationship type constraints, are not supported!"
	 		)
	 }
	
	
	private def String printHeader(String modelname) '''
	// 
	// This solidity files is automatically written by the FRaMED Solidity generator
	//
	// The following features are not supported:
	//       - Compartment inheritance, because too many weird things can happen, which are not checked in FRaMED
	//
	//       - Compartments that play a role in another compartment, because on object level it would enforce a new
	//         compartment instance, i.e. a new individual on                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             meta level
	//
	//       - intra relationship constraints like reflexive or symmetric, since actually that are constraints on the
	//         objects that play the roles and not on the roles themselves.
	//
	// SPDX-License-Identifier: MIT	
	pragma solidity >=0.7.0 <0.8.0;		
'''

	private def String printNaturalType() '''
		«section("The declaration of all natural types that occur in the model")»
		contract «makeClassName("NaturalType") + " {"»
		   struct  «makeStructName("NaturalType") + " {"» 
					«naturalTypes.join("", "\n", "\n", [ naturalType |  "string " + makeClassName(naturalType) + ";"] )»
			}
		}
	'''
	
	private def String makeClassName(String str) {
		return "rosi" + str
	}
	
	private def String makeStructName(String str) {
		return "structRosi" + str
	}
	
	
	private def String section(String title) '''
		
		
		
		«String.join("", Collections.nCopies(headingWidth, "//"))»
		// «title» «if (title.length+4 <= headingWidth) repeat(" ", headingWidth - title.length - 4) + "//" else ""»
		«String.join("", Collections.nCopies(headingWidth, "//"))»
		
	'''
	
	
	//debug output for CROM
	private def void debug(){
		println(modelInfo)
	}
	
	 /**
	 * This methods prints some information about the CROM which can be used for example as debug output.
	 */
	private def String modelInfo() '''
		
		CROM:
		=====
		
		===Natural types:===
		«if (naturalTypes.empty) "none"
			else naturalTypes.join("",",\n","\n", [ nt | nt ])»
		
		===Compartment types:===
		«if (compartmentTypes.empty) "none"
			else compartmentTypes.join("",",\n","\n", [ ct | ct ])»
		
		===Role Types:===
		«if (roleTypes.empty) "none"
			else roleTypes.join("",",\n","\n", [ rt | rt ])»
		
		===Relationship types:===
		«if (relationshipTypes.empty) "none"
			else relationshipTypes.join("",",\n","\n", [ rst | rst ])»
		
		===Natural type inheritance:===
		«if (crom.ntinh.empty) "none"
			else crom.ntinh.join("", ",\n", "\n", [elem | elem.key + " -> " + elem.value])»
		
		===Compartment type inheritance:===
		«if (crom.ctinh.empty) "none"
			else crom.ctinh.join("", ",\n", "\n", [elem | elem.key + " -> " + elem.value])»
		
		===Fills relation:===
		«if (crom.fills.empty) "none"
			else crom.fills.join("", "\n", "\n", [ elem | elem.key.key + " -> " + elem.value + " in " + elem.key.value])»
		
		===Domain and range of relationship types:===
		«crom.rel.entrySet.join("", "\n", "\n",
			[elem | elem.key.key + ": " + elem.value.key + " -> " + elem.value.value + " in " + elem.key.value ])»
		
		Constraint Model:
		=================
		
		===Cardinality constraints for relationship types:===
		«if (crom.card.empty) "none"
			else crom.card.entrySet.join("", "\n", "\n", 
				[elem | elem.key.key + " in " + elem.key.value + ": " + elem.value.key + " ----> " + elem.value.value ])»
		
		===Occurrence constraints for role types and role groups===
		«if (crom.rolec.empty) "none"
			else crom.rolec.entrySet.join("", "\n", "\n", 
				[ elem | elem.value.join("In " + elem.key + ":\n", "\n", "\n", [ x | x.key + " -> " + x.value])])»
		
		===Occurrence constraints, filtered for role groups (with their generated names)===
		«if (occurrenceConstraintsForRoleGroups.empty) "none"
			else occurrenceConstraintsForRoleGroups.entrySet.join("", "\n", "\n", 
				[ elem | elem.value.join("In " + elem.key + ":\n", "\n", "\n", [ x | x.key + " -> " + x.value.name])])»
		
		===Top level role groups===
		«topLevelRoleGroups.join("", "\n", "\n", [ rg | rg.name ])»
		
		===Names of role groups===
		«rgNames.entrySet.join("\n", [ entry | entry.key + " --> " + entry.value])»
		
		===Intra relationship type constraints:===
		«if (crom.intra.empty) "none"
			else crom.intra»
		
		===Inter relationship type constraints:===
		«if (crom.inter.empty) "none"
			else crom.inter»
	'''
	
		/**
	 * Given a role group, this method retrieves the associated name.
	 */
	private def String getName(RoleGroup roleGroup) {
		if (rgNames.containsValue(roleGroup))
			rgNames.filter[ name, rg | rg.equals(roleGroup) ].entrySet.head.key
		else ""
	}
	
		
	
	
}