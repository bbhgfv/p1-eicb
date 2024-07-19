/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.parsing;

import mavlc.errors.SyntaxError;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;

import static mavlc.parsing.Token.TokenType.*;
import static mavlc.syntax.expression.Compare.Comparison.*;

/* TODO enter group information
 *
 * EiCB group number: 090
 * Names and matriculation numbers of all group members:
 * Thien Nam Vu tv40nusa
 * Quoc Dang Huy Nguyen qn97cuhi
 * Dieu Linh Nguyen dn23qace
 */

/**
 * A recursive-descent parser for MAVL.
 */
public final class Parser {
	
	private final Deque<Token> tokens;
	private Token currentToken;
	
	/**
	 * @param tokens A token stream that was produced by the {@link Scanner}.
	 */
	public Parser(Deque<Token> tokens) {
		this.tokens = tokens;
		currentToken = tokens.poll();
	}
	
	/**
	 * Parses the MAVL grammar's start symbol, Module.
	 *
	 * @return A {@link Module} node that is the root of the AST representing the tokenized input program.
	 * @throws SyntaxError to indicate that an unexpected token was encountered.
	 */
	public Module parse() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Function> functions = new ArrayList<>();
		List<RecordTypeDeclaration> records = new ArrayList<>();
		while(currentToken.type != EOF) {
			switch(currentToken.type) {
				case FUNCTION:
					functions.add(parseFunction());
					break;
				case RECORD:
					records.add(parseRecordTypeDeclaration());
					break;
				default:
					throw new SyntaxError(currentToken, FUNCTION, RECORD);
			}
		}
		return new Module(location, functions, records);
	}
	
	private String accept(Token.TokenType type) {
		Token t = currentToken;
		if(t.type != type)
			throw new SyntaxError(t, type);
		acceptIt();
		return t.spelling;
	}
	
	private void acceptIt() {
		currentToken = tokens.poll();
		if(currentToken == null || currentToken.type == ERROR)
			throw new SyntaxError(currentToken != null ? currentToken : new Token(EOF, null, -1, -1));
	}
	
	private Function parseFunction() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FUNCTION);
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		List<FormalParameter> parameters = new ArrayList<>();
		List<Statement> body = new ArrayList<>();
		
		accept(LPAREN);
		if(currentToken.type != RPAREN) {
			parameters.add(parseFormalParameter());
			while(currentToken.type != RPAREN) {
				accept(COMMA);
				parameters.add(parseFormalParameter());
			}
		}
		accept(RPAREN);
		
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			body.add(parseStatement());
		accept(RBRACE);
		
		return new Function(location, name, typeSpecifier, parameters, body);
	}
	
	private FormalParameter parseFormalParameter() {
		SourceLocation location = currentToken.sourceLocation;
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		return new FormalParameter(location, name, typeSpecifier);
	}
	
	private RecordTypeDeclaration parseRecordTypeDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		
		accept(RECORD);
		String name = accept(ID);
		accept(LBRACE);
		List<RecordElementDeclaration> elements = new ArrayList<>();
		// no empty records allowed
		elements.add(parseRecordElementDeclaration());
		while(currentToken.type != RBRACE) {
			elements.add(parseRecordElementDeclaration());
		}
		accept(RBRACE);
		
		return new RecordTypeDeclaration(location, name, elements);
	}
	
	private RecordElementDeclaration parseRecordElementDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean isVariable;
		switch(currentToken.type) {
			case VAL:
				acceptIt();
				isVariable = false;
				break;
			case VAR:
				acceptIt();
				isVariable = true;
				break;
			default:
				throw new SyntaxError(currentToken, VAL, VAR);
		}
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		accept(SEMICOLON);
		
		return new RecordElementDeclaration(location, isVariable, typeSpecifier, name);
	}

	/**
	 * parse an iterator declaration statement
	 * @return an Iteratordeclaration
	 */
	private IteratorDeclaration parseIteratorDeclaration() {
		// TODO implement (task 1.5)
		SourceLocation location = currentToken.sourceLocation;
		boolean isVar;
		if(currentToken.type == VAL){
			isVar=false;
			accept(VAL);
		}else{
			isVar=true;
			accept(VAR);
		}
		TypeSpecifier type = parseTypeSpecifier();
		String name = accept(ID);
		return new IteratorDeclaration(location,name,type,isVar);
	}
	
	private TypeSpecifier parseTypeSpecifier() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean vector = false;
		switch(currentToken.type) {
			case INT:
				acceptIt();
				return new IntTypeSpecifier(location);
			case FLOAT:
				acceptIt();
				return new FloatTypeSpecifier(location);
			case BOOL:
				acceptIt();
				return new BoolTypeSpecifier(location);
			case VOID:
				acceptIt();
				return new VoidTypeSpecifier(location);
			case STRING:
				acceptIt();
				return new StringTypeSpecifier(location);
			case VECTOR:
				accept(VECTOR);
				vector = true;
				break;
			case MATRIX:
				accept(MATRIX);
				break;
			case ID:
				String name = accept(ID);
				return new RecordTypeSpecifier(location, name);
			default:
				throw new SyntaxError(currentToken, INT, FLOAT, BOOL, VOID, STRING, VECTOR, MATRIX, ID);
		}
		
		accept(LANGLE);
		TypeSpecifier subtype;
		switch(currentToken.type) {
			case INT:
				subtype = new IntTypeSpecifier(currentToken.sourceLocation);
				break;
			case FLOAT:
				subtype = new FloatTypeSpecifier(currentToken.sourceLocation);
				break;
			default:
				throw new SyntaxError(currentToken, INT, FLOAT);
		}
		acceptIt();
		accept(RANGLE);
		accept(LBRACKET);
		Expression x = parseExpr();
		accept(RBRACKET);
		
		if(vector)
			return new VectorTypeSpecifier(location, subtype, x);
		
		accept(LBRACKET);
		Expression y = parseExpr();
		accept(RBRACKET);
		
		return new MatrixTypeSpecifier(location, subtype, x, y);
	}
	
	private Statement parseStatement() {
		switch(currentToken.type) {
			case VAL:
				return parseValueDef();
			case VAR:
				return parseVarDecl();
			case RETURN:
				return parseReturn();
			case ID:
				return parseAssignOrCall();
			case FOR:
				return parseFor();
			case FOREACH:
				return parseForEach();
			case IF:
				return parseIf();
			case SWITCH:
				return parseSwitch();
			case LBRACE:
				return parseCompound();
			default:
				throw new SyntaxError(currentToken, VAL, VAR, RETURN, ID, FOR, FOREACH, IF, SWITCH, LBRACE);
		}
	}

	/**
	 * parse the value definition statement
	 * @return a ValueDefinition class
	 */
	private ValueDefinition parseValueDef() {
		// TODO implement (task 1.1)
		SourceLocation location = currentToken.sourceLocation;
		accept(VAL);
		TypeSpecifier type  = parseTypeSpecifier();
		String valName = accept(ID);
		accept(ASSIGN);
		Expression val = parseExpr();
		accept(SEMICOLON);
		return new ValueDefinition(location,type,valName,val);


	}

	/**
	 * parse the variable declaration statement
	 * @return a VariableDeclaration class
	 */
	private VariableDeclaration parseVarDecl() {
		// TODO implement (task 1.1)
		SourceLocation location = currentToken.sourceLocation;
		accept(VAR);
		TypeSpecifier type = parseTypeSpecifier();
		String varname = accept(ID);
		accept(SEMICOLON);
		return new VariableDeclaration(location,type,varname);
	}

	/**
	 * parse a return statement
	 * @return a ReturnStatement
	 */
	private ReturnStatement parseReturn() {
		// TODO implement (task 1.6)
		SourceLocation location = currentToken.sourceLocation;
		accept(RETURN);
		Expression expression = parseExpr();
		accept(SEMICOLON);
		return new ReturnStatement(location,expression);
	}

	/**
	 * parse an assign or call statement
	 * @return a Statement
	 */
	private Statement parseAssignOrCall() {
		// TODO implement (task 1.6)
		SourceLocation location = currentToken.sourceLocation;
		String id = accept(ID);
		if(currentToken.type==LPAREN) {
			CallExpression callExpression = parseCall(id,location);
			accept(SEMICOLON);
			return new CallStatement(location,callExpression);
		}else {
			Statement statement = parseAssign(id,location);
			accept(SEMICOLON);
			return statement;
		}
	}

	/**
	 * parse the assignment statement
	 * @param name name of the val or var
	 * @param location location of the currentToken
	 * @return
	 */
	private VariableAssignment parseAssign(String name, SourceLocation location) {
		// TODO implement (task 1.1)
		if(currentToken.type==LBRACKET){
			accept(LBRACKET);
			Expression expression1 = parseExpr();
			accept(RBRACKET);
			if(currentToken.type==LBRACKET){
				accept(LBRACKET);
				Expression expression2 = parseExpr();
				accept(RBRACKET);
				accept(ASSIGN);
				Expression val = parseExpr();
				return new VariableAssignment(location, new MatrixLhsIdentifier(location,name,expression2,expression1), val);
			}else {
				accept(ASSIGN);
				Expression val = parseExpr();
				return new VariableAssignment(location, new VectorLhsIdentifier(location,name,expression1), val);
			}
		}
		else if(currentToken.type==AT){
			accept(AT);
			String id = accept(ID);
			accept(ASSIGN);
			Expression val = parseExpr();
			return new VariableAssignment(location, new RecordLhsIdentifier(location,name,id), val);

		}else {
			accept(ASSIGN);
			Expression val = parseExpr();
			return new VariableAssignment(location, new LeftHandIdentifier(location, name), val);
		}
	}

	/**
	 * parse a call statement
	 * @param name name of the function statement
	 * @param location location of the currentToken
	 * @return a CallExpression
	 */
	private CallExpression parseCall(String name, SourceLocation location) {
		// TODO implement (task 1.6)
		accept(LPAREN);
		List<Expression> expressionList = new ArrayList<>();
		if(currentToken.type==RPAREN){
			accept(RPAREN);
		}else {
			Expression expression = parseExpr();
			expressionList.add(expression);
			while (currentToken.type==COMMA){
				accept(COMMA);
				Expression expression1 = parseExpr();
				expressionList.add(expression1);
			}
			accept(RPAREN);
		}
		return new CallExpression(location,name,expressionList);
	}

	/**
	 * parse a for-loop statement
	 * @return an ForLoop
	 */
	private ForLoop parseFor() {
		// TODO implement (task 1.4)
		SourceLocation location = currentToken.sourceLocation;
		accept(FOR);
		accept(LPAREN);
		String initvarname = accept(ID);
		accept(ASSIGN);
		Expression initExpression = parseExpr();
		accept(SEMICOLON);
		Expression loopCondition = parseExpr();
		accept(SEMICOLON);
		String increVarname = accept(ID);
		accept(ASSIGN);
		Expression increExpression = parseExpr();
		accept(RPAREN);
		Statement statement = parseStatement();
		 return new ForLoop(location,initvarname,initExpression,loopCondition,increVarname,increExpression,statement);
	}

	/**
	 * parse a foreach-loop statement
	 * @return a ForEachLoop
	 */
	private ForEachLoop parseForEach() {
		// TODO implement (task 1.5)
		SourceLocation location = currentToken.sourceLocation;
		accept(FOREACH);
		accept(LPAREN);
		IteratorDeclaration iteratorDeclaration = parseIteratorDeclaration();
		accept(COLON);
		Expression expression = parseExpr();
		accept(RPAREN);
		Statement statement = parseStatement();
		return new ForEachLoop(location,iteratorDeclaration,expression,statement);
	}
	
	private IfStatement parseIf() {
		SourceLocation location = currentToken.sourceLocation;
		accept(IF);
		accept(LPAREN);
		Expression test = parseExpr();
		accept(RPAREN);
		Statement then = parseStatement();
		if(currentToken.type == ELSE) {
			acceptIt();
			return new IfStatement(location, test, then, parseStatement());
		}
		return new IfStatement(location, test, then);
	}

	/**
	 * parse switch statement
	 * @return a SwitchStatement
	 */
	private SwitchStatement parseSwitch() {
		// TODO implement (task 1.7)
		SourceLocation location = currentToken.sourceLocation;
		accept(SWITCH);
		accept(LPAREN);
		Expression expression = parseExpr();
		accept(RPAREN);
		List<Case> caseList = new ArrayList<>();
		List<Default> defaultList = new ArrayList<>();
		accept(LBRACE);
		while (currentToken.type==CASE || currentToken.type == DEFAULT){
			Token.TokenType type = currentToken.type;
			if(type==CASE){
				Case caseTemp = parseCase();
				caseList.add(caseTemp);
			}else {
				Default defaultTemp = parseDefault();
				defaultList.add(defaultTemp);
			}
		}
		accept(RBRACE);
		return new SwitchStatement(location,expression,caseList,defaultList);
	}

	/**
	 * parse a case statement
	 * @return a Case
	 */
	private Case parseCase() {
		// TODO implement (task 1.7)
		SourceLocation location = currentToken.sourceLocation;
		accept(CASE);
		Expression expression = parseExpr();
		accept(COLON);
		Statement statement = parseStatement();
		return new Case(location,expression,statement);
	}
	/**
	 * parse a default statement
	 * @return a Default
	 */
	private Default parseDefault() {
		// TODO implement (task 1.7)
		SourceLocation location = currentToken.sourceLocation;
		accept(DEFAULT);
		accept(COLON);
		Statement statement = parseStatement();
		return new Default(location,statement);	}
	
	private CompoundStatement parseCompound() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Statement> statements = new ArrayList<>();
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			statements.add(parseStatement());
		accept(RBRACE);
		
		return new CompoundStatement(location, statements);
	}
	
	private Expression parseExpr() {
		return parseSelect();
	}
	
	private Expression parseSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression cond = parseOr();
		if(currentToken.type == QMARK) {
			acceptIt();
			Expression trueCase = parseOr();
			accept(COLON);
			Expression falseCase = parseOr();
			return new SelectExpression(location, cond, trueCase, falseCase);
		}
		return cond;
	}
	
	private Expression parseOr() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAnd();
		while(currentToken.type == OR) {
			acceptIt();
			x = new Or(location, x, parseAnd());
		}
		return x;
	}
	
	private Expression parseAnd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseNot();
		while(currentToken.type == AND) {
			acceptIt();
			x = new And(location, x, parseNot());
		}
		return x;
	}

	/**
	 * parse a not statement
	 * @return an Expression
	 */
	private Expression parseNot() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == NOT) {
			acceptIt();
			Expression x = parseCompare();
			// TODO replace null by call to the appropriate parse method (task 1.2)
			return new Not(location, x);
		}
		// TODO replace null by call to the appropriate parse method (task 1.2)
		Expression x = parseCompare();
		return x;
	}

	/**
	 * parse the comparation statement
	 * @return an Expression
	 */
	private Expression parseCompare() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;
		Expression x = parseAddSub();
		while (currentToken.type == RANGLE || currentToken.type == LANGLE || currentToken.type == CMPLE || currentToken.type == CMPGE || currentToken.type == CMPEQ || currentToken.type == CMPNE){
			Token.TokenType operatorType = currentToken.type;
			acceptIt();
			Expression y = parseAddSub();
			switch (operatorType) {
				case LANGLE:
					x = new Compare(location,x,y,LESS);
					break;
				case RANGLE:
					x = new Compare(location,x,y,GREATER);
					break;
				case CMPLE:
					x = new Compare(location,x,y,LESS_EQUAL);
					break;
				case CMPGE:
					x = new Compare(location,x,y,GREATER_EQUAL);
					break;
				case CMPEQ:
					x = new Compare(location,x,y,EQUAL);
					break;
				case CMPNE:
					x = new Compare(location,x,y,NOT_EQUAL);
					break;
			}
		}
		return x;
	}

	/**
	 * parse a addition or subtraction statement
	 * @return an Expression
	 */
	private Expression parseAddSub() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;
		Expression x = parseMulDiv();
		while (currentToken.type == ADD || currentToken.type == SUB){
			Token.TokenType operatorType = currentToken.type;
			acceptIt();
			Expression y = parseMulDiv();
			if(operatorType == ADD){
				x = new Addition(location,x,y);
			}else {
				x= new Subtraction(location,x,y);
			}
		}
		return x;
	}

	/**
	 * parse a Multiply or Division statement
	 * @return an Expression
	 */
	private Expression parseMulDiv() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;
		Expression x = parseUnaryMinus();
		while (currentToken.type == MULT || currentToken.type == DIV){
			Token.TokenType operatorType = currentToken.type;
			acceptIt();
			Expression y = parseUnaryMinus();
			if(operatorType == MULT){
				x = new Multiplication(location,x,y);
			}else {
				x= new Division(location,x,y);
			}
		}
		return x;
	}

	/**
	 * parse an unary minus statement
	 * @return an Expression
	 */
	private Expression parseUnaryMinus() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == SUB) {
			acceptIt();
			// TODO replace null by call to the appropriate parse method (task 1.2)
			Expression x = parseExponentiation();
			return new UnaryMinus(location, x);
		}
		// TODO replace null by call to the appropriate parse method (task 1.2)
		Expression x = parseExponentiation();
		return x;
	}

	/**
	 * parse an exponential statement
	 * @return an Expression
	 */
	private Expression parseExponentiation() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;
		Expression x = parseDotProd();
		while (currentToken.type == EXP){
			Token.TokenType operatorType = currentToken.type;
			acceptIt();
			Expression y = parseDotProd();
			x = new Exponentiation(location,x,y);
		}
		return x;
	}
	
	private Expression parseDotProd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseMatrixMul();
		while(currentToken.type == DOTPROD) {
			acceptIt();
			x = new DotProduct(location, x, parseMatrixMul());
		}
		return x;
	}
	
	private Expression parseMatrixMul() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseTranspose();
		while(currentToken.type == MATMULT) {
			acceptIt();
			x = new MatrixMultiplication(location, x, parseTranspose());
		}
		return x;
	}

	/**
	 * parse a transpose statement
	 * @return an Expression
	 */
	private Expression parseTranspose() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == TRANSPOSE) {
			acceptIt();
			// TODO replace null by call to the appropriate parse method (task 1.2)
			Expression x = parseDim();
			 return new MatrixTranspose(location, x);
		}
		// TODO replace null by call to the appropriate parse method (task 1.2)
		Expression x = parseDim();
		return x;
	}
	
	private Expression parseDim() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseSubRange();
		switch(currentToken.type) {
			case ROWS:
				acceptIt();
				return new MatrixRows(location, x);
			case COLS:
				acceptIt();
				return new MatrixCols(location, x);
			case DIM:
				acceptIt();
				return new VectorDimension(location, x);
			default:
				return x;
		}
	}
	
	private Expression parseSubRange() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseElementSelect();
		
		if(currentToken.type == LBRACE) {
			acceptIt();
			Expression xStartIndex = parseExpr();
			accept(COLON);
			Expression xBaseIndex = parseExpr();
			accept(COLON);
			Expression xEndIndex = parseExpr();
			accept(RBRACE);
			if(currentToken.type != LBRACE)
				return new SubVector(location, x, xBaseIndex, xStartIndex, xEndIndex);
			
			accept(LBRACE);
			Expression yStartIndex = parseExpr();
			accept(COLON);
			Expression yBaseIndex = parseExpr();
			accept(COLON);
			Expression yEndIndex = parseExpr();
			accept(RBRACE);
			return new SubMatrix(location, x, xBaseIndex, xStartIndex, xEndIndex, yBaseIndex, yStartIndex, yEndIndex);
		}
		
		return x;
	}

	/**
	 * parse an element selection statement
	 * @return and Expression
	 */
	private Expression parseElementSelect() {
		// TODO implement (task 1.3)
		SourceLocation location = currentToken.sourceLocation;
		Expression x = parseRecordElementSelect();
		while (currentToken.type == LBRACKET){
			accept(LBRACKET);
			Expression y = parseExpr();
			accept(RBRACKET);
			x = new ElementSelect(location,x,y);
		}
		return x;

	}
	
	private Expression parseRecordElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAtom();
		
		if(currentToken.type == AT) {
			accept(AT);
			String elementName = accept(ID);
			x = new RecordElementSelect(location, x, elementName);
		}
		
		return x;
	}
	
	private Expression parseAtom() {
		SourceLocation location = currentToken.sourceLocation;
		
		switch(currentToken.type) {
			case INTLIT:
				return new IntValue(location, parseIntLit());
			case FLOATLIT:
				return new FloatValue(location, parseFloatLit());
			case BOOLLIT:
				return new BoolValue(location, parseBoolLit());
			case STRINGLIT:
				return new StringValue(location, accept(STRINGLIT));
			default: /* check other cases below */
		}
		
		if(currentToken.type == ID) {
			String name = accept(ID);
			if(currentToken.type != LPAREN) {
				return new IdentifierReference(location, name);
				
			} else {
				return parseCall(name, location);
			}
		}
		
		if(currentToken.type == LPAREN) {
			acceptIt();
			Expression x = parseExpr();
			accept(RPAREN);
			return x;
		}
		
		if(currentToken.type == AT) {
			acceptIt();
			String name = accept(ID);
			return new RecordInit(location, name, parseInitializerList());
		}
		
		if(currentToken.type == LBRACKET) {
			return new StructureInit(location, parseInitializerList());
		}
		
		throw new SyntaxError(currentToken, INTLIT, FLOATLIT, BOOLLIT, STRINGLIT, ID, LPAREN, LBRACKET, AT);
	}
	
	private List<Expression> parseInitializerList() {
		List<Expression> elements = new ArrayList<>();
		
		accept(LBRACKET);
		elements.add(parseExpr());
		while(currentToken.type == COMMA) {
			accept(COMMA);
			elements.add(parseExpr());
		}
		accept(RBRACKET);
		
		return elements;
	}
	
	private int parseIntLit() {
		return Integer.parseInt(accept(INTLIT));
	}
	
	private float parseFloatLit() {
		return Float.parseFloat(accept(FLOATLIT));
	}
	
	private boolean parseBoolLit() {
		return Boolean.parseBoolean(accept(BOOLLIT));
	}
}
