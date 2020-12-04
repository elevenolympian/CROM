package org.rosi.crom.toformal.popup.actions;

import org.rosi.crom.toformal.generator.SolidityCodeGenerator;

public class GenerateSolidityCodeAction extends AbstractGenerateAction {
	public GenerateSolidityCodeAction() {
		super();
		generator = new SolidityCodeGenerator();
	}
}
