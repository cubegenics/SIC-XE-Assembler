#include<bits/stdc++.h>
using namespace std;

set<string> opCodes, directives, type1, type2, type3, special, registers;

map<string,map<string,int>> address;
//addresses for equ, labels and literals

map<char,int> registerValue;

map<string,string> code;
map<string,string> registerCode;

map<string,map<string,string>> equTable;

vector<string> errors;
vector<string> headerNames;

map<string,vector<string>> records;

vector<pair<string,string>> listing;
vector<pair<string,string>> objectCodes;

map<string,set<string>> references;

set<string> definitions;

map<string,int> sectionSize;

bool firstExecutablePresent=0;

string lowerCase(string s){
    for(int i=0;i<(int)s.size();i++){
        if(s[i]!='\'')s[i]=tolower(s[i]);
        else{
            int ind=i+1;
            while(ind<(int)s.size() && s[ind]!='\''){
                ind++;
            }
            i=ind;
        }
    }
    return s;
}

string upperCase(string s){
    for(int i=0;i<(int)s.size();i++){
        if(s[i]!='\'')s[i]=toupper(s[i]);
        else{
            int ind=i+1;
            while(ind<(int)s.size() && s[ind]!='\''){
                ind++;
            }
            i=ind;
        }
    }
    return s;
}

bool absoluteTerm(string s){
    for(auto x: s){
        if((int)(x-'0')>=0 && (int)(x-'0')<=9)continue;
        else return 0;
    }
    return 1;
}

vector<string> decomposeTerms(string s){
    //also breaks at commas
    //e.x. equ buffend-buffer+100,x -> buffend | - | buffer | + | 100 | , | x
    vector<string> el;
    string tmp="";
    int i=0;
    int n=(int)s.size();
    while(i<n && s[i]==' ')i++;
    if(s[i]=='+'){
        while(i<n && s[i]!=' '){
            tmp=tmp+s[i];
            i++;
        }
        el.push_back(tmp);
        tmp.clear();
    }
    while(i<n){
        if(s[i]==' '){
            if(!tmp.empty())el.push_back(tmp);
            tmp.clear();
            while(i<n && s[i]==' ')i++;
        }
        if(s[i]=='+' || s[i]=='-' || s[i]==',' || s[i]=='/' || s[i]=='*'){
            if(!tmp.empty())el.push_back(tmp);
            tmp.clear();
            el.push_back(tmp+s[i]);
            i++;
        }
        while(i<n && s[i]==' ')i++;
        if(i<n)tmp=tmp+s[i];
        i++;
    }
    if(!tmp.empty())el.push_back(tmp);
    return el;
}

void debugDecomposeTerms(){
    string s;
    vector<string> el;
    //input is like: buffend-buffer+1000 etc.

    while(1){
        getline(cin,s);
        if(s=="done")break;
        el=decomposeTerms(s);
        cout<<"s: "<<'\n';
        cout<<"elements: ";
        for(auto x: el){
            cout<<x<<" | ";
        }
        cout<<'\n';
    }
}



bool representIn24Bits(int x){
    return (abs(x)<=((1<<23)-1));//leftmost bit would be sign bit
}

bool validExpressionUpdated(string s, string currentSection){
    //input is just an expression such as lab1-lab2+10/2 etc.
    //lab1, x not allowed here

    //dealt with #label1-label2+10/2 etc.
    //dealt with @label1-label2+10/2 etc.
    vector<string> terms=decomposeTerms(s);
    int com=0;
    for(auto x: terms){
        if(x==",")com++;
    }
    if(com)return 0;//can't have any commas


    set<string> operators;
    operators.insert("-");
    operators.insert("+");
    operators.insert("/");
    operators.insert("*");

    int rel=0;

    for(int i=0;i<(int)terms.size();i++){
        if(references[currentSection].count(terms[i]))continue;
        if(operators.count(terms[i])){
            if(!i)return 0;//expression can't begin with an operator
            if(i==(int)terms.size()-1)return 0;//expression can't end at an operator
            //can't have consecutive operators
            if(operators.count(terms[i-1]))return 0;//can't have consecutive operators
        }else{//dealt with commas already
            //label or absolute value
            if(absoluteTerm(terms[i])){
                //absolute term
                if(i){
                    if(operators.count(terms[i-1]));//fine
                    else return 0;//error
                }
            }else{
                //label
                if(!i){
                    if(terms[i][0]=='#' || terms[i][0]=='@'){
                        if(!absoluteTerm(terms[i].substr(1)))rel++;
                        continue;
                    }else{
                        if(!absoluteTerm(terms[i]))rel++;
                    }
                }else{//must have an operand before               
                    if(terms[i][0]=='#' || terms[i][0]=='@')return 0;
                    if(terms[i-1]=="+")rel++;
                    else if(terms[i-1]=="-")rel--;
                    else if(terms[i-1]=="/" || terms[i-1]=="*")return 0;
                    else return 0;//no operand before this label
                }

            }
        }
    }

    if(rel>1 || rel<0)return 0;
    return 1;
}

bool validExpressionType(string s, string currentSection){
    //for lda lab1-lab2+10/2-lab3+5*4+ref etc.
    //for label lda etc., deal accordingly by erasing the label before calling this function

    vector<string> terms=decomposeTerms(s);
    if(terms.empty())return 0;
    terms.erase(terms.begin());   

    if(terms[0]=="-" || terms[0]=="+" || terms[0]=="/" || terms[0]=="*" || terms[0]==",")return 0;

    if(terms.back()=="x"){
        if(terms.size()==1)return 0;//can't have like lda x
        else{
            if(terms[terms.size()-2]!=",")return 0;//must be lda exp, x
            int com=0;
            for(int i=0;i<(int)terms.size();i++){
                if(terms[i]==",")com++;
            }
            if(com!=1)return 0;//there must be exactly one comma 
        }
        terms.pop_back();//remove x
        terms.pop_back();//remove ,
    }
    //now on, it'll be the same expression for equ or anything.
    string tmp;
    for(auto x: terms){
        tmp=tmp+x;
        tmp=tmp+" ";
    }
    return validExpressionUpdated(tmp,currentSection);

}

void initializeType1(){
    vector<string> tmp={"fix","float","hio","norm","sio","tio"};
    vector<string> codes={"C4","C0","F4","C8","F0","F8"};
    for(int i=0;i<(int)tmp.size();i++){
        type1.insert(tmp[i]);
        opCodes.insert(tmp[i]);
        code[tmp[i]]=codes[i];
    }
}

void initializeType2(){
    vector<string> tmp={"addr","clear","compr","divr","mulr","rmo","shiftl","shiftr","subr","svc","tixr"};
    vector<string> codes={"90","B4","A0","9C","98","AC","A4","A8","94","B0","B8"};
    for(int i=0;i<(int)tmp.size();i++){
        type2.insert(tmp[i]);
        opCodes.insert(tmp[i]);
        code[tmp[i]]=codes[i];
    }
    /*
    addr r1,r2
    clear r1
    compr r1,r2
    divr r1,r2
    mulr r1,r2
    rmo r1,r2
    shiftl r1,n
    shiftr f1,n
    subr r1,r2
    svc n
    tixr r1
    */
}

void initializeType3andType4(){
    vector<string> tmp={"add","addf","and","comp","compf","div","divf","j","jeq","jgt","jlt","jsub","lda","ldb","ldch","ldf","ldl","lds","ldt","ldx","lps","mul","mulf","or","rd","rsub","ssk","sta","stb","stch","stf","sti","stl","sts","stsw","stt","stx","sub","subf","td","tix","wd"};
    vector<string> codes={"18","58","40","28","88","24","64","3C","30","34","38","48","00","68","50","70","08","6C","74","04","D0","20","60","44","D8","4C","EC","0C","78","54","80","D4","14","7C","E8","84","10","1C","5C","E0","2C","DC"};
    for(int i=0;i<(int)tmp.size();i++){
        type3.insert(tmp[i]);
        opCodes.insert(tmp[i]);
        opCodes.insert('+'+tmp[i]);
        code[tmp[i]]=codes[i];
    }
    /*
    add m
    addf m
    and m
    comp m
    compf m
    div m
    divf m
    j m
    jeq m
    jgt m
    jlt m
    jsub m
    lda m
    ldb m
    ldch m
    ldf m
    ldl m
    lds m
    ldt m
    ldx m
    lps m
    mul m
    mulf m
    or m
    rd m
    rsub
    ssk m
    sta m
    stb m 
    stch m
    stf m
    sti m
    stl m
    sts m
    stsw m
    stt m
    stx m
    sub m
    subf m
    td m
    tix m
    wd m
    */
}

void initializeDirectives(){
    vector<string> tmp={"resw","resb","word","byte"};
    for(auto x: tmp){
        directives.insert(x);
    }
}

void initializeRegisters(){
    vector<string> tmp={"a","x","l","b","s","t","f"};
    string r="";
    for(int i=0;i<(int)tmp.size();i++){
        string x=tmp[i];
        registers.insert(x);
        char c=x[0];
        registerValue[c]=-1;
        registerCode[r+c]=r+char('0'+i);
    }
}

void init(){
    initializeType1();
    initializeType2();
    initializeType3andType4();
    initializeDirectives();
    initializeRegisters();
    special.insert("ltorg");
    special.insert("org");
}

bool checkFormatType1(vector<string> el){
    for(auto x: el)x=lowerCase(x);
    if((int)el.size()!=1)return 0;
    if(type1.find(el[0])!=type1.end())return 1;
    return 0;
}

bool checkFormatType2(vector<string> el){
    for(auto x: el)x=lowerCase(x);
    if((int)el.size()==2){
        /*
        clear r1
        svc n
        tixr r1
        */
        string reg=el[1];
        if(el[0]=="clear"){
            if(registers.find(reg)!=registers.end())return 1;
            else return 0;
        }else if(el[0]=="svc"){
            if(absoluteTerm(reg))return 1;
            else return 0;
        }else if(el[0]=="tixr"){
            if(registers.find(reg)!=registers.end())return 1;
            else return 0;
        }else return 0;
    }else if((int)el.size()==3){
        if(el[0]=="shiftl" || el[0]=="shiftr"){
            /*
            NOTE: assuming 'n' is a decimal integer
            shiftl r1,n
            shiftr r1,n
            */ 
            string reg1=el[1];
            if(registers.find(reg1)!=registers.end()){
                if(absoluteTerm(el[2]))return 1;//n must be an integer, doesn't check number of bits though
                else return 0;
            }else return 0;
        }else if(type2.find(el[0])!=type2.end()){
            /*
            addr r1,r2
            compr r1,r2
            divr r1,r2
            mulr r1,r2
            rmo r1,r2
            subr r1,r2
            */
            bool valid=1;
            for(int i=1;i<3;i++){
                if(registers.find(el[i])==registers.end())valid=0;
            }
            return valid;
        }else return 0;
    }else return 0;
    /*
    addr r1,r2
    clear r1
    compr r1,r2
    divr r1,r2
    mulr r1,r2
    rmo r1,r2
    shiftl r1,n
    shiftr r1,n
    subr r1,r2
    svc n
    tixr r1
    */
}

bool checkFormatType3(string vec){
    /*
    add m
    addf m
    and m
    comp m
    compf m
    div m
    divf m
    j m
    jeq m
    jgt m
    jlt m
    jsub m
    lda m
    ldb m
    ldch m
    ldf m
    ldl m
    lds m
    ldt m
    ldx m
    lps m
    mul m
    mulf m
    or m
    rd m
    rsub
    ssk m
    sta m
    stb m 
    stch m
    stf m
    sti m
    stl m
    sts m
    stsw m
    stt m
    stx m
    sub m
    subf m
    td m
    tix m
    wd m
    */
    vector<string> el=decomposeTerms(vec);
    if((int)el.size()==1){
        if(el[0]=="rsub")return 1;
        else return 0;
    }
    if(el[0]=="rsub")return 0;//rsub can't contain anything else in the instruction

    if(!type3.count(el[0]))return 0;

    //check format
    vector<string> terms=decomposeTerms(vec);
    if(terms.empty())return 0;
    terms.erase(terms.begin());   

    if(terms[0]=="-" || terms[0]=="+" || terms[0]=="/" || terms[0]=="*" || terms[0]==",")return 0;

    if(terms.back()=="x"){
        if(terms.size()==1)return 0;//can't have like lda x
        else{
            if(terms[terms.size()-2]!=",")return 0;//must be lda exp, x
            int com=0;
            for(int i=0;i<(int)terms.size();i++){
                if(terms[i]==",")com++;
            }
            if(com!=1)return 0;//there must be exactly one comma 
        }
        terms.pop_back();//remove x
        terms.pop_back();//remove ,
    }
    //now on, it'll be the same expression for equ or anything.
    string tmp;
    for(auto x: terms){
        tmp=tmp+x;
        tmp=tmp+" ";
    }


    terms=decomposeTerms(tmp);
    int com=0;
    for(auto x: terms){
        if(x==",")com++;
    }
    if(com)return 0;//can't have any commas


    set<string> operators;
    operators.insert("-");
    operators.insert("+");
    operators.insert("/");
    operators.insert("*");

    int rel=0;

    for(int i=0;i<(int)terms.size();i++){
        if(operators.count(terms[i])){
            if(!i)return 0;//expression can't begin with an operator
            if(i==(int)terms.size()-1)return 0;//expression can't end at an operator
            //can't have consecutive operators
            if(operators.count(terms[i-1]))return 0;//can't have consecutive operators
        }else{//dealt with commas already
            //label or absolute value
            if(absoluteTerm(terms[i])){
                //absolute term
                if(i){
                    if(operators.count(terms[i-1]));//fine
                    else return 0;//error
                }
            }else{
                //label
                if(!i)rel++;
                else{//must have an operand before               
                    if(terms[i][0]=='#' || terms[i][0]=='@')return 0;
                    if(terms[i-1]=="+")rel++;
                    else if(terms[i-1]=="-")rel--;
                    else if(terms[i-1]=="/" || terms[i-1]=="*")return 0;
                    else return 0;//no operand before this label
                }

            }
        }
    }

    return 1;
}

bool checkFormatType4(string vec){
    //can we have +rsub?
    //currently assuming we can't
    vector<string> el=decomposeTerms(vec);
    if(el[0][0]=='+'){
        el[0].erase(el[0].begin());
        string tmp;
        for(int i=0;i<(int)el.size();i++){
            tmp=tmp+el[i];
            tmp=tmp+" ";
        }
        return checkFormatType3(tmp);
    }else return 0;
}

bool checkFormatWord(string vec){
    /*
    word lab1-lab2+100
    word lab1-lab2
    word 1000
    are all valid
    */
    //input format: maxlen word buffer-bufend
    vector<string> terms=decomposeTerms(vec);
    terms.erase(terms.begin());
    if(terms[0]!="word")return 0;
    string expression;
    for(int i=1;i<(int)terms.size();i++){
        expression=expression+terms[i];
        expression=expression+" ";
    }
    if((int)terms.size()==1)return 0;//can't have just "word"
    if((int)terms.size()==2){
        //must be word 100 or some absolute value
        if(!absoluteTerm(terms[1])){
            //can also be word buffend
            return 1;
            //check that the label is valid in pass2 for word
        }
        else if(absoluteTerm(terms[1]) && representIn24Bits(stoi(terms[1])))return 1;//word 100
        else return 0;
    }else{
        //must be an expression
        
        vector<string> terms=decomposeTerms(expression);
        int com=0;
        for(auto x: terms){
            if(x==",")com++;
        }
        if(com)return 0;//can't have any commas


        set<string> operators;
        operators.insert("-");
        operators.insert("+");
        operators.insert("/");
        operators.insert("*");

        int rel=0;

        for(int i=0;i<(int)terms.size();i++){
            if(operators.count(terms[i])){
                if(!i)return 0;//expression can't begin with an operator
                if(i==(int)terms.size()-1)return 0;//expression can't end at an operator
                //can't have consecutive operators
                if(operators.count(terms[i-1]))return 0;//can't have consecutive operators
            }else{//dealt with commas already
                //label or absolute value
                if(absoluteTerm(terms[i])){
                    //absolute term
                    if(i){
                        if(operators.count(terms[i-1]));//fine
                        else return 0;//error
                    }
                }else{
                    //label
                    if(!i)rel++;
                    else{//must have an operand before               
                        if(terms[i][0]=='#' || terms[i][0]=='@')return 0;
                        if(terms[i-1]=="+")rel++;
                        else if(terms[i-1]=="-")rel--;
                        else if(terms[i-1]=="/" || terms[i-1]=="*")return 0;
                        else return 0;//no operand before this label
                    }

                }
            }
        }
        return 1;
    }

}

vector<string> decomposeElements(string s){
    //only breaks at spaces or commas, not + or - or * etc.

    /*
    +jsub is one element
    #value is one element
    @label is one element
    c'eof' is stored as  c'eof'
    x'f1' is stored as x'f1'
    buffer,x are two separate elements 
    essentially, we can only break at spaces or commas
    note: equ bufend-buffer is stored as a single element iff there are no spaces.
    update: spaces for these equ like expressions are now handled in decomposeTerms()
    keep that in mind
    */

    vector<string> elements;
    string tmp;
    int i=0;
    int n=(int)s.size();
    while(i<n){
        if(s[i]==' '){
            if(!tmp.empty())elements.push_back(tmp);
            tmp.clear();
            while(i<n && s[i]==' ')i++;
        }
        if(i==n)break;
        if(s[i]==','){
            i++;
            elements.push_back(tmp);
            tmp.clear();
            continue;
        }
        tmp=tmp+s[i];
        i++;
        if(i==n)elements.push_back(tmp);
    }
    return elements;
}

void debugDecomposeElements(){
    while(1){
        string s;
        getline(cin,s);
        if(s=="done")break;
        vector<string> el=decomposeElements(s);
        cout<<"s: "<<s<<'\n';
        cout<<"elements: ";
        for(auto x: el){
            cout<<x<<" | ";
        }
        cout<<'\n';
    }
}

vector<string> takeInstructions(){
    vector<string> vec;
    vector<string> elements;
    string s;
    while(1){
        getline(cin,s);
        vec.push_back(s);
        elements=decomposeElements(s);
        if(lowerCase(elements[0])=="end")break;       
    }
    return vec;
}

string intToHex(int x){
    stringstream s;
    s<<hex<<x;
    string ans(s.str());
    ans=upperCase(ans);
    bool neg=0;
    //check correctness of this method
    while(ans[0]=='F'){
        if((int)ans.size()==3){
            neg=0;
            break;
        }
        neg=1;
        ans.erase(ans.begin());
    }
    if(neg)ans='F'+ans;
    return ans;
}

int hexToInt(string s){
    int ans;
    stringstream tmp;
    tmp<<s;
    tmp>>hex>>ans;
    return ans;
}

bool validHex(string s){
    //takes input as x'f1' etc.
    for(int i=2;i<(int)s.size()-1;i++){
        char x=s[i];
        if((int)(x-'0')>=0 && (int)(x-'0')<=9)continue;
        if((int)(x-'a')>=0 && (int)(x-'a')<=5)continue;
        if((int)(x-'A')>=0 && (int)(x-'A')<=5)continue;
        else return 0;
    }
    return 1;
}


void pass1(vector<string> vec){
    //assuming instructions are not empty
    for(auto &x: vec)x=lowerCase(x);
    vector<string> elements=decomposeElements(vec[0]);
    if(elements[0].size()>6){
        string er="Program Name can't be of length more than 6 columns";
        errors.push_back(er);
    }
    headerNames.push_back(elements[0]);

    if((int)elements.size()<2){
        string er="Incomplete first instruction: \"";
        er=er+vec[0];
        er=er+"\"";
        errors.push_back(er);
    }

    //what if opcode is not start, initialize locctr to 0??
    int locctr=0;
    if((int)elements.size()>2){
        locctr=hexToInt(elements[2]);//assuming starting address specified
    }
    if((int)elements.size()>1 && elements[1]!="start"){
        string er="Invalid instruction format, START not found: \"";
        er=er+vec[0];
        er=er+"\"";
        errors.push_back(er);
    }
    int previousLocctr=locctr;
    int start=0;
    string startingAddress="0";
    if((int)elements.size()>2)startingAddress=elements[2];
    if((int)elements.size()>2 && !absoluteTerm(elements[2])){
        string er="Invalid starting address : \"";
        er=er+vec[0];
        er=er+"\"";
        errors.push_back(er);
    }else if((int)elements.size()>2 && absoluteTerm(elements[2])){
        start=hexToInt(elements[2]);
    }
    vector<int> addresses;
    vector<string> literalBuffer;
    listing.push_back({intToHex(locctr),vec[0]});
    string currentSection=elements[0];
    for(int i=1;i<(int)vec.size();i++){
        addresses.push_back(locctr);
        listing.push_back({intToHex(locctr),vec[i]});
        previousLocctr=locctr;
        elements=decomposeElements(vec[i]);
        string first=elements[0];
        if(lowerCase(first)=="base"){//can we use just base? or need to use base m
            //nothing to do in pass 1 for base
            continue;
        }
        if(lowerCase(first)=="nobase"){
            continue;
        }
        if(opCodes.find(first)!=opCodes.end()){
            //opcode
            //wrong if multiple opcodes
            int type;
            if(first[0]=='+' && type3.find(first.substr(1))!=type3.end()){//find opcode for type 4 excluding the +
                if(!checkFormatType4(vec[i])){
                    string er="Incorrect Format for instruction: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                }
                locctr+=4;
                type=4;
            }else if(type1.find(first)!=type1.end()){
                if(!checkFormatType1(elements)){
                    string er="Incorrect Format for instruction: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                }
                locctr+=1;
                type=1;
            }else if(type2.find(first)!=type2.end()){
                if(!checkFormatType2(elements)){
                    string er="Incorrect Format for instruction: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                }
                locctr+=2;
                type=2;
            }else if(type3.find(first)!=type3.end()){
                if(!checkFormatType3(vec[i])){
                    string er="Incorrect Format for instruction: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                }
                locctr+=3;
                type=3;
            }else{
                string er="Invalid opcode found: \"";
                er=er+vec[i];
                er=er+"\"";
                errors.push_back(er);
                //error invalid opcode
            }
            //insert any literal found into the literal buffer
            //can only use literals for type3/4 instructions
            for(int j=1;j<(int)elements.size();j++){
                if(elements[j][0]=='='){
                    //this element is a literal
                    literalBuffer.push_back(elements[j]);
                    if(type!=3 && type!=4){
                        string er="Can't use literals for instructions of type ";
                        er=er+to_string(type);
                        errors.push_back(er);
                    }
                }
            }

        }else if(special.find(first)!=special.end()){
            //ltorg, org
            if(lowerCase(first)=="ltorg"){
                if((int)elements.size()!=1){
                    string er="Invalid instruction format for instruction: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                    continue;
                }
                //assign all literals addresses, then proceed
                set<string> processedX, processedC;
                for(auto x: literalBuffer){
                    int bytes;
                    if(tolower(x[1])=='x'){
                        if(processedX.count(x))continue;
                        bytes=x.size()-4;
                        if(bytes&1){
                            //error
                            string er="Incorrect format for hexadecimal in literal: \"";
                            er=er+x;
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }else{
                            bytes/=2;
                        }
                        processedX.insert(x);
                    }else if(tolower(x[1])=='c'){
                        if(processedC.count(x))continue;
                        bytes=x.size()-4;
                        processedC.insert(x);
                    }else{
                        //error, invalid literal
                        string er="Invalid literal found: \"";
                        er=er+x;
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                    address[currentSection][x]=locctr;//address[=c'eof'] is assigned
                    listing.push_back({intToHex(locctr),x});
                    locctr+=bytes;
                }
                literalBuffer.clear();
            }else if(lowerCase(first)=="org"){
                //assumed org only occurs as org and org label, nothing else
                if(elements.size()==1){
                    //org 
                    locctr=previousLocctr;
                }else if(elements.size()==2){
                    //org value 
                    previousLocctr=locctr;
                    locctr=address[currentSection][elements[1]];//address of a label
                    //check that elements[1] is a valid previously defined label
                }else{
                    //error
                    string er="Invalid format for instruction: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                }
            }
        }else if(first=="extref"){
            //error if some external reference being used 
            //isn't actually declared as an external definition
            //in the other control section
            (listing.back()).first="";
            for(int i=1;i<(int)elements.size();i++){
                if((int)elements[i].size()>6){
                    string er="Label of length greater than 6 columns is not allowed: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                    break;
                }
                references[currentSection].insert(elements[i]);//references for a control section
            }
        }else if(first=="extdef"){
            (listing.back()).first="";
            for(int i=1;i<(int)elements.size();i++){
                if((int)elements[i].size()>6){
                    string er="Label of length greater than 6 columns is not allowed: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                    break;
                }
                definitions.insert(elements[i]);
            }
            //there'll also be an error if some label in some control section
            //coincides with an external definition
        }else{
            //label or error as well for wrong opcode 
            //include case for second string being equ as well, alongside opcode
            if(address[currentSection].count(first)){
                //duplicate label, error
                string er="Duplicate label found in instruction: \"";
                er=er+vec[i];
                er=er+"\"";
                errors.push_back(er);
            }else{
                address[currentSection][first]=locctr;
            }

            if((int)elements.size()==1 && first!="end"){
                string er="Incomplete instruction, only label found: \"";
                er=er+vec[i];
                er=er+"\"";
                errors.push_back(er);
                continue;
            }
            
            string label=first;
            string second=elements[1];
            //elements[1] must exist
            
            if((int)label.size()>6){
                string er="Label of length greater than 6 columns is not allowed: \"";
                er=er+vec[i];
                er=er+"\"";
                errors.push_back(er);
                continue;
            }

            //second is either opcode or equ
            //or resw resb word byte 
            first=second;
            elements.erase(elements.begin());

            //same processing as that of a statement without a label is carried out hereafter
            if(first=="csect"){

                address[currentSection].erase(label);

                headerNames.push_back(label);

                definitions.insert(label);

                pair<string,string> popped=listing.back();
                listing.pop_back();

                set<string> processedX, processedC;
                for(auto x: literalBuffer){
                    int bytes;
                    if(tolower(x[1])=='x'){
                        if(processedX.count(x))continue;
                        bytes=x.size()-4;
                        if(bytes&1){
                            string er="Incorrect format for hexadecimal in literal: \"";
                            er=er+x;
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }else{
                            bytes/=2;
                        }
                        processedX.insert(x);
                    }else if(tolower(x[1])=='c'){
                        if(processedC.count(x))continue;
                        bytes=x.size()-4;
                        processedC.insert(x);
                    }else{
                        string er="Invalid literal found: \"";
                        er=er+x;
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                    address[currentSection][x]=locctr;//address[=c'eof'] is assigned
                    listing.push_back({intToHex(locctr),x});
                    locctr+=bytes;
                }
                literalBuffer.clear();
                //inserting literals of previous control section appropriately

                sectionSize[currentSection]=locctr-start;

                start=0;


                listing.push_back({intToHex(locctr),vec[i]});

                currentSection=label;
                address[currentSection][label]=0;
                locctr=0;
                (listing.back()).first="0";
                //do extref and extdef have to come immediately after the line of csect??
            }else if(opCodes.find(first)!=opCodes.end()){
                //opcode
                //wrong if multiple opcodes
                int type;
                if(first[0]=='+' && type3.find(first.substr(1))!=type3.end()){//find opcode for type 4 excluding the +
                    vector<string> terms=decomposeTerms(vec[i]);
                    terms.erase(terms.begin());
                    string tmp="";
                    for(auto x: terms){
                        //just converting a vector of string to an instruction format i.e. one long string
                        tmp=tmp+x;
                        tmp=tmp+" ";
                    }
                    if(!checkFormatType4(tmp)){
                        string er="Incorrect Format for instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                    }
                    locctr+=4;
                    type=4;
                }else if(type1.find(first)!=type1.end()){
                    if(!checkFormatType1(elements)){
                        string er="Incorrect Format for instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                    }
                    locctr+=1;
                    type=1;
                }else if(type2.find(first)!=type2.end()){
                    if(!checkFormatType2(elements)){
                        string er="Incorrect Format for instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                    }
                    locctr+=2;
                    type=2;
                }else if(type3.find(first)!=type3.end()){
                    string tmp="";
                    vector<string> terms=decomposeTerms(vec[i]);
                    terms.erase(terms.begin());
                    for(auto x: terms){
                        //just converting a vector of string to an instruction format i.e. one long string
                        tmp=tmp+x;
                        tmp=tmp+" ";
                    }
                    if(!checkFormatType3(tmp)){
                        string er="Incorrect Format for instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                    }
                    locctr+=3;
                    type=3;
                }else{
                    string er="Invalid opcode found: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                }
                //insert any literal found into the literal buffer
                //can only use literals for type3/4 instructions
                for(int j=1;j<(int)elements.size();j++){
                    if(elements[j][0]=='='){
                        //this element is a literal
                        //check that it's a valid literal???
                        literalBuffer.push_back(elements[j]);
                        if(type!=3 && type!=4){
                            string er="Can't use literals for instructions of type ";
                            er=er+to_string(type);
                            errors.push_back(er);
                        }
                    }
                }
                // if(firstExecutable.empty()){
                //     firstExecutable=label;
                // }
            }else if(directives.find(first)!=directives.end()){
                //assembler directive
                if(elements.size()!=2){
                    string er="Incomplete instruction \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                    continue;
                }
                if(lowerCase(first)=="resb"){
                    //check that bytes should be valid
                    if(!absoluteTerm(elements[1])){
                        string er="Number of bytes asked to reserve is not an absolute term for instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                    int bytes=stoi(elements[1]);
                    locctr+=bytes;
                }else if(lowerCase(first)=="resw"){
                    //check that bytes should be valid
                    if(!absoluteTerm(elements[1])){
                        string er="Number of bytes asked to reserve is not an absolute term for instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                    int bytes=stoi(elements[1]);
                    locctr+=3*bytes;
                }else if(lowerCase(first)=="byte"){
                    //check that bytes should be valid
                    int bytes=(int)elements[1].size()-3;//remove 'x' or 'c' and two quotes

                    if(elements[1][0]=='x'){
                        if(bytes&1){
                            string er="Incorrect format for hexadecimal in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }else{
                            bytes/=2;
                        }
                    //assuming you can only use hexadecimal digits while using x'f1' etc.
                        if(!validHex(elements[1].substr(2,bytes))){
                            string er="Incorrect format for hexadecimal in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                    }
                    
                    locctr+=bytes;
                }else if(lowerCase(first)=="word"){
                    //check that word fits in 3 bytes
                    
                    if(!checkFormatWord(vec[i])){
                        //word bufend-buffer
                        //word bufend
                        //word lab1-lab2+100
                        //word 1000 are valid
                        string er="Invalid value for word in instruction : \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                    locctr+=3;
                }

                
            }else if(lowerCase(first)=="equ"){
                //check that the expression for equ is valid
                if((int)elements.size()==1){
                    string er="Incomplete instruction: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    continue;
                }
                if(elements[1]=="*"){
                    address[currentSection][label]=locctr;
                    equTable[currentSection][label]=intToHex(locctr);
                }else{
                    //some expression
                    if(!validExpressionUpdated(elements[1],currentSection)){//is elements[1] valid as input to function???
                        //evaluate validity of expression, alternate operators and operands, no / * etc. with relative operands
                        string er="Invalid Expression in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }else{
                        //decompose into terms: "bufend", "-", "buffer", "+", 1000
                        //what about equ #maxlen??
                        //check that you can't have forward references

                        bool errorFlag=0;
                        vector<string> el=decomposeTerms(vec[i]);
                        for(int i=2;i<(int)el.size();i++){
                            if(el[i]=="+" || el[i]=="-")continue;
                            if(el[i]=="/" || el[i]=="*")continue;
                            if(absoluteTerm(el[i]))continue;
                            //label
                            //must not be forward reference
                            if(!address[currentSection].count(el[i]) || el[i]==","){
                                //label not encountered yet
                                //error
                                errorFlag=1;
                            }
                        }

                        if(errorFlag){
                            string er="Unknown label or forward reference found in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        vector<string> terms=decomposeTerms(elements[1]);
                        if((int)terms.size()==1){
                            //cases where we would just replace equ with the term it represents
                            if(terms[0][0]=='#'){
                                if(absoluteTerm(terms[0].substr(1))){
                                    //like equ #4096
                                    equTable[currentSection][label]=terms[0].substr(1);
                                }else{
                                    //like equ maxlen
                                    if(address[currentSection].count(terms[0])){
                                        equTable[currentSection][label]=terms[0];
                                    }else{
                                        string er="Previously undefined symbol in EQU instruction: \"";
                                        er=er+vec[i];
                                        er=er+"\"";
                                        errors.push_back(er);
                                        continue;
                                    }
                                }
                            }else{  
                                //can you do equ @maxlen or something? I assume you can't
                                //here, we enter if it's like: label equ maxlen 
                                //assuming, you can also represent registers as maxlen equ A
                                if(address[currentSection].count(terms[0])){
                                    equTable[currentSection][label]=terms[0];
                                }else if(absoluteTerm(terms[0])){
                                    equTable[currentSection][label]=terms[0];
                                }else if(registers.count(terms[0])){
                                    equTable[currentSection][label]=terms[0];
                                }else{
                                    string er="Previously undefined symbol in EQU instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }
                            if(absoluteTerm(equTable[currentSection][label])){
                                (listing.back()).first=equTable[currentSection][label];
                            }else if(equTable[currentSection].count(equTable[currentSection][label])){
                                //resolving a series of definitions
                                //for e.x.
                                //lab equ 10
                                //bufend equ lab
                                //maxlen equ bufend
                                //maxlen would then represent the value 10
                                string value=equTable[currentSection][label];
                                while(equTable[currentSection].count(value)){
                                    value=equTable[currentSection][value];
                                }
                                if(absoluteTerm(value)){
                                    (listing.back()).first=value;
                                    equTable[currentSection][label]=value;
                                }else if(address[currentSection].count(value)){
                                    (listing.back()).first=address[currentSection][value];
                                    equTable[currentSection][label]=address[currentSection][value];
                                }else if(registers.count(value)){
                                    (listing.back()).first=upperCase(value);
                                    equTable[currentSection][label]=value;
                                }
                            }else if(address[currentSection].count(equTable[currentSection][label])){
                                (listing.back()).first=intToHex(address[currentSection][equTable[currentSection][label]]);
                            }else if(registers.count(equTable[currentSection][label])){
                                (listing.back()).first=upperCase(equTable[currentSection][label]);
                            }
                            continue;
                        }
                        int value=0;
                        //since there are no forward references, equ can be evaluated to an
                        //some value
                        for(int x=0;x<(int)terms.size();x++){
                            if(terms[x]=="+" || terms[x]=="-")continue;
                            if(terms[x]=="/" || terms[x]=="*")continue;
                            int tmp;
                            if(absoluteTerm(terms[x])){
                                tmp=stoi(terms[x]);
                                if(x+1<(int)terms.size()){
                                    if(terms[x+1]=="/"){
                                        if(x+2<(int)terms.size() && absoluteTerm(terms[x+2])){
                                            int v1=stoi(terms[x]), v2=stoi(terms[x+2]);       
                                            if(!v2){
                                                errorFlag=1;
                                                break;
                                            }
                                            tmp=v1/v2;
                                            if(!x){
                                                value+=tmp;
                                            }else{
                                                if(terms[x-1]=="+")value+=tmp;
                                                else if(terms[x-1]=="-")value-=tmp;
                                            }
                                            x+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else if(terms[x+1]=="*"){
                                        if(x+2<(int)terms.size() && absoluteTerm(terms[x+2])){
                                            int v1=stoi(terms[x]), v2=stoi(terms[x+2]);
                                            tmp=v1*v2;
                                            if(!x){
                                                value+=tmp;
                                            }else{
                                                if(terms[x-1]=="+")value+=tmp;
                                                else if(terms[x-1]=="-")value-=tmp;
                                            }
                                            x+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }                                           
                                    }else{
                                        //+ or -
                                        if(!x){
                                            value+=tmp;
                                            continue;
                                        }
                                        if(terms[x-1]=="+")value+=tmp;
                                        else if(terms[x-1]=="-")value-=tmp;
                                    }
                                }else{
                                    if(!x){
                                        value+=tmp;
                                        continue;
                                    }
                                    if(terms[x-1]=="+")value+=tmp;
                                    else if(terms[x-1]=="-")value-=tmp;
                                }
                                continue;
                            }else{
                                //label
                                if(address[currentSection].count(terms[x])){
                                    tmp=address[currentSection][terms[x]];
                                }else if(references[currentSection].count(terms[x])){
                                    tmp=0;
                                    //generate modification record
                                }else{
                                    string er="Invalid label found in equ instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    break;
                                }
                            }
                            if(!x){
                                value+=tmp;
                            }else{
                                if(terms[x-1]=="+")value+=tmp;
                                else value-=tmp;
                            }
                        }
                        if(errorFlag){
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        equTable[currentSection][label]=to_string(value);
                        (listing.back()).first=intToHex(value);
                    }
                }
            }else{
                if(label!="end"){
                    string er="Invalid instruction: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                }else{
                    if(elements.empty())continue;
                    else{
                        firstExecutablePresent=1;
                    }
                    // if(first!=firstExecutable || firstExecutable.empty()){
                    //     string er="Incorrect first executable instruction in end";
                    //     errors.push_back(er);
                    // }
                }
            }
        }
    }
    //assign all literals addresses
    set<string> processedX, processedC;
    for(auto x: literalBuffer){
        int bytes;
        if(tolower(x[1])=='x'){
            if(processedX.count(x))continue;
            bytes=x.size()-4;
            if(bytes&1){
                //error
                string er="Incorrect format for hexadecimal in literal: \"";
                er=er+x;
                er=er+"\"";
                errors.push_back(er);
                continue;
            }else{
                bytes/=2;
            }
            processedX.insert(x);
        }else if(tolower(x[1])=='c'){
            if(processedC.count(x))continue;
            bytes=x.size()-4;
            processedC.insert(x);
        }else{
            //error, invalid literal
            string er="Invalid literal found: \"";
            er=er+x;
            er=er+"\"";
            errors.push_back(er);
            continue;
        }
        address[currentSection][x]=locctr;//address[=c'eof'] is assigned
        listing.push_back({intToHex(locctr),x});
        locctr+=bytes;
    }
    literalBuffer.clear();
    sectionSize[currentSection]=locctr-start;
}

string padded(string a, int n, bool right, char c){
    if((int)a.size()==n)return a;
    n-=(int)a.size();
    if(right){
        for(int i=0;i<n;i++)a=a+c;
    }else{
        for(int i=0;i<n;i++)a=c+a;
    }
    return a;
}

void modificationRecordType4(string sectionName, vector<string> terms, int idx){
    //input format : +lda #100-lab1+lab2-lab4+...
    terms.erase(terms.begin());//remove opcode

    int rel=0;
    string label;

    if(terms[0][0]=='#' || terms[0][0]=='@')terms[0].erase(terms[0].begin());

    for(int i=0;i<(int)terms.size();i++){
        if(terms[i]=="," || terms[i]=="+" || terms[i]=="-" || terms[i]=="/" || terms[i]=="*" || terms[i]=="x")continue;
        if(absoluteTerm(terms[i]))continue;
        if(references[sectionName].count(terms[i])){
            string s;
            s=s+'M';
            int var=hexToInt(listing[idx].first);
            var++;
            string tmp=intToHex(var);
            tmp=padded(tmp,6,0,'0');
            s=s+tmp;
            s=s+"05";
            if(!i){
                s=s+'+';
            }else{
                s=s+terms[i-1];
            }
            tmp=terms[i];
            tmp=padded(tmp,6,1,'_');
            s=s+tmp;
            records[sectionName].push_back(s);
        }else{
            if(!i)rel++;
            else{
                if(terms[i-1]=="+"){
                    rel++;
                    label=terms[i];
                }else if(terms[i-1]=="-")rel--;
            }
        }
    }
    if(rel==1){
        string s;
        s=s+'M';
        int var=hexToInt(listing[idx].first);
        var++;
        string tmp=intToHex(var);
        tmp=padded(tmp,6,0,'0');
        s=s+tmp;
        s=s+"05";
        s=s+'+';
        tmp=sectionName;
        tmp=padded(tmp,6,1,'_');
        s=s+tmp;
        records[sectionName].push_back(s);
    }
}

void modificationRecordWord(string sectionName, vector<string> terms, int idx){
    //input format: word expression
    terms.erase(terms.begin());
    int rel=0;
    string label;

    if(terms[0][0]=='#' || terms[0][0]=='@')terms[0].erase(terms[0].begin());
    for(int i=0;i<(int)terms.size();i++){
        if(terms[i]=="," || terms[i]=="+" || terms[i]=="-" || terms[i]=="/" || terms[i]=="*" || terms[i]=="x")continue;
        if(absoluteTerm(terms[i]))continue;
        if(references[sectionName].count(terms[i])){
            string s;
            s=s+'M';
            string tmp=listing[idx].first;
            tmp=padded(tmp,6,0,'0');
            s=s+tmp;
            s=s+"06";
            if(!i){
                s=s+'+';
            }else{
                s=s+terms[i-1];
            }
            tmp=terms[i];
            tmp=padded(tmp,6,1,'_');
            s=s+tmp;
            records[sectionName].push_back(s);
        }else{
            if(!i)rel++;
            else{
                if(terms[i-1]=="+"){
                    rel++;
                    label=terms[i];
                }else if(terms[i-1]=="-")rel--;
            }
        }
    }
    if(rel==1){
        string s;
        s=s+'M';
        string tmp=listing[idx].first;
        tmp=padded(tmp,6,0,'0');
        s=s+tmp;
        s=s+"06";
        s=s+'+';
        tmp=sectionName;
        tmp=padded(tmp,6,1,'_');
        s=s+tmp;
        records[sectionName].push_back(s);
    }
}

void pass2(vector<string> vec){//returns the object program 
    for(auto &x: vec)x=lowerCase(x);
    vector<string> elements=decomposeElements(vec[0]);
    vector<string> terms;

    string startingAddress="0";
    if((int)elements.size()>2){
        startingAddress=padded(elements[2],6,0,'0');
    }
    objectCodes.push_back({vec[0],""});
    int locctr=0;
    if((int)elements.size()>=3)locctr=hexToInt(elements[2]);
    int pc;
    string currentSection=elements[0];
    for(int i=1;i<(int)vec.size();i++){
        locctr=hexToInt(listing[i].first);
        if(i!=(int)vec.size()-1)pc=hexToInt(listing[i+1].first);
        elements=decomposeElements(vec[i]);
        terms=decomposeTerms(vec[i]);
        string first=elements[0];
        string objectCode;
        for(auto &x: elements){
            if(address[currentSection].count(x))continue;
            if(equTable[currentSection].count(x)){
                x=equTable[currentSection][x];//replacing all symbols with their actual values
            }else if((x[0]=='#' || x[0] =='@') && equTable[currentSection].count(x.substr(1))){
                x=x[0]+equTable[currentSection][x.substr(1)];
            }
        }
        
        for(int i=1;i<(int)terms.size();i++){
            // if(address[currentSection].count(terms[i]))continue;
            if(equTable[currentSection].count(terms[i])){
                terms[i]=equTable[currentSection][terms[i]];//replacing all symbols with their actual values
            }else if((terms[i][0]=='#' || terms[i][0] =='@') && equTable[currentSection].count(terms[i].substr(1))){
                terms[i]=terms[i][0]+equTable[currentSection][terms[i].substr(1)];
            }
        }
        if(first=="extref"){
            for(int j=1;j<(int)elements.size();j++){
                if(!definitions.count(elements[j])){
                    string er="Undefined Reference in instruction: \"";
                    er=er+vec[i];
                    er=er+"\"";
                    errors.push_back(er);
                }
            }
            objectCodes.push_back({vec[i],""});
            //anything used as an extref must be defined somewhere
            continue;
        }else if(first=="extdef"){
            //definitions already inserted in pass1 
            objectCodes.push_back({vec[i],""});
            continue;
        }else if((int)elements.size()>1 && elements[1]=="csect"){
            currentSection=first;
        }else if(address[currentSection].find(first)!=address[currentSection].end()){
            //instruction with a label
            if((int)elements.size()==1){
                //only possible for literals
                //literal size what
                if(first[1]=='x'){
                    objectCode=first.substr(3,(int)first.size()-4);
                }else if(first[1]=='c'){
                    for(int k=3;k<(int)first.size()-1;k++){
                        objectCode=objectCode+intToHex((int)first[k]);
                    }
                }
                objectCodes.push_back({vec[i],objectCode});
                continue;
            }
            string label=first;
            first=elements[1];
            elements.erase(elements.begin());

            if(opCodes.find(first)!=opCodes.end()){
                //check that external references and other labels 
                //must be of the same control section
                // vector<string> terms=decomposeTerms(vec[i]);
                terms.erase(terms.begin());//remove label
                if(terms[0]=="+"){
                    terms.erase(terms.begin());
                    terms[0]='+'+terms[0];
                }
                if(first[0]=='+'){
                    //modification record
                    bitset<2> ni;
                    bitset<4> xbpe;
                    xbpe[0]=1;
                    int displacement=0;
                    string addr;
                    if(terms[1][0]=='#'){//+op #m-a+b..
                        ni[0]=1;
                        if(absoluteTerm(terms[1].substr(1))){//lda #100-..+..
                            //check that absolute value must fit in 5 hex bits
                            int value=stoi(terms[1].substr(1));
                            string inHex=intToHex(value);
                            if((int)inHex.size()<=5){
                                displacement=value;
                                addr=intToHex(displacement);
                                addr=padded(addr,5,0,'0');
                            }else{
                                string er="Immediate value too large for 20 bits in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                            //first term checked. move on to the remaining labels in the expression
                            string tmp;
                            for(int i=1;i<(int)terms.size();i++){
                                tmp=tmp+terms[i];
                                tmp=tmp+" ";
                            }
                            if(terms.back()=="x"){
                                string er="Can't do indexed mode of addressing with immediate mode: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                            if(validExpressionUpdated(tmp,currentSection)){
                                bool errorFlag=0;
                                int comma=0;
                                for(int i=2;i<(int)terms.size();i++){
                                    if(terms[i]=="-" || terms[i]=="+")continue;
                                    if(terms[i]=="/" || terms[i]=="*")continue;
                                    if(terms[i]==","){
                                        comma++;
                                        break;
                                    }
                                    //label must be present in the csect or must be a reference
                                    if(address[currentSection].count(terms[i])){
                                        //fine
                                        if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                        else if(terms[i-1]=="-")displacement=displacement-(address[currentSection][terms[i]]);
                                    }else if(references[currentSection].count(terms[i])){
                                        //fine 
                                    }else if(absoluteTerm(terms[i])){
                                        int tmp=stoi(terms[i]);
                                        if(i+1<(int)terms.size()){
                                            if(terms[i+1]=="/"){
                                                if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                    int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                    if(!v2){
                                                        errorFlag=1;
                                                        break;
                                                    }
                                                    tmp=v1/v2;
                                                    if(i==2)displacement+=tmp;
                                                    else{
                                                        if(terms[i-1]=="+")displacement+=tmp;
                                                        else if(terms[i-1]=="-")displacement-=tmp;
                                                    }
                                                    i+=2;
                                                }else{
                                                    errorFlag=1;
                                                    break;
                                                }
                                            }else if(terms[i+1]=="*"){
                                                if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                    int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                    tmp=v1*v2;
                                                    if(i==2){
                                                        displacement+=tmp;
                                                    }else{
                                                        if(terms[i-1]=="+")displacement+=tmp;
                                                        else if(terms[i-1]=="-")displacement-=tmp;
                                                    }
                                                    i+=2;
                                                }else{
                                                    errorFlag=1;
                                                    break;
                                                }
                                            }else{
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                            }
                                        }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }
                                    }else errorFlag=1;
                                }
                                addr=intToHex(displacement);
                                addr=padded(addr,5,0,'0');
                                if(comma){
                                    string er="Invalid expression in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                                if(errorFlag){
                                    string er="Undefined label in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }else{
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }else if(address[currentSection].count(terms[1].substr(1))){
                            displacement=address[currentSection][terms[1].substr(1)];
                            addr=intToHex(address[currentSection][terms[1].substr(1)]);
                            addr=padded(addr,5,0,'0');
                            terms[1].erase(terms[1].begin());
                            string tmp;
                            for(int i=1;i<(int)terms.size();i++){
                                tmp=tmp+terms[i];
                                tmp=tmp+" ";
                            }
                            if(terms.back()=="x"){
                                string er="Can't do indexed mode of addressing with immediate mode: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                            if(validExpressionUpdated(tmp,currentSection)){
                                bool errorFlag=0;
                                int comma=0;
                                for(int i=2;i<(int)terms.size();i++){
                                    if(terms[i]=="-" || terms[i]=="+")continue;
                                    if(terms[i]=="/" || terms[i]=="*")continue;
                                    if(terms[i]==","){
                                        comma++;
                                        continue;
                                    }
                                    //label must be present in the csect or must be a reference
                                    if(address[currentSection].count(terms[i])){
                                        //fine
                                        if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                        else if(terms[i-1]=="-")displacement=displacement-(address[currentSection][terms[i]]);
                                    }else if(references[currentSection].count(terms[i])){
                                        //fine 
                                    }else if(absoluteTerm(terms[i])){
                                        int tmp=stoi(terms[i]);
                                        if(i+1<(int)terms.size()){
                                            if(terms[i+1]=="/"){
                                                if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                    int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                    if(!v2){
                                                        errorFlag=1;
                                                        break;
                                                    }
                                                    tmp=v1/v2;
                                                    if(i==2)displacement+=tmp;
                                                    else{
                                                        if(terms[i-1]=="+")displacement+=tmp;
                                                        else if(terms[i-1]=="-")displacement-=tmp;
                                                    }
                                                    i+=2;
                                                }else{
                                                    errorFlag=1;
                                                    break;
                                                }
                                            }else if(terms[i+1]=="*"){
                                                if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                    int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                    tmp=v1*v2;
                                                    if(i==2){
                                                        displacement+=tmp;
                                                    }else{
                                                        if(terms[i-1]=="+")displacement+=tmp;
                                                        else if(terms[i-1]=="-")displacement-=tmp;
                                                    }
                                                    i+=2;
                                                }else{
                                                    errorFlag=1;
                                                    break;
                                                }
                                            }else{
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                            }
                                        }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }

                                    }else errorFlag=1;
                                }
                                addr=intToHex(displacement);
                                addr=padded(addr,5,0,'0');
                                if(comma){
                                    string er="Invalid expression in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                                if(errorFlag){
                                    string er="Undefined label in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }else{
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }else if(references[currentSection].count(terms[1].substr(1))){
                            addr=padded(addr,5,0,'0');
                            displacement=0;
                            terms[1].erase(terms[1].begin());
                        }else{
                            string er="Label not found in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                    }else if(terms[1][0]=='@'){//+op @m
                        ni[1]=1;
                        if(address[currentSection].count(terms[1].substr(1))){
                            //what to do here e.x. +jsub @wrrec
                            displacement=address[currentSection][terms[1].substr(1)];
                            addr=intToHex(address[currentSection][terms[1].substr(1)]);
                            addr=padded(addr,5,0,'0');
                            terms[1].erase(terms[1].begin());
                        }else if(references[currentSection].count(terms[1].substr(1))){
                            //fine
                        }else if(absoluteTerm(terms[1].substr(1))){
                            displacement=stoi(terms[1].substr(1));
                            //does displacement matter
                        }else{
                            string er="Label not found in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        string tmp;
                        for(int i=1;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }
                        if(terms.back()=="x"){
                            string er="Can't do indexed mode of addressing with indirect mode: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        if(validExpressionUpdated(tmp,currentSection)){
                            bool errorFlag=0;
                            int comma=0;
                            for(int i=2;i<(int)terms.size();i++){
                                if(terms[i]=="-" || terms[i]=="+")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;
                                if(terms[i]==","){
                                    comma++;
                                    continue;
                                }
                                //label must be present in the csect or must be a reference
                                if(address[currentSection].count(terms[i])){
                                    //fine
                                    if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                    if(terms[i-1]=="-")displacement=displacement+(address[currentSection][terms[i]]);
                                }else if(references[currentSection].count(terms[i])){
                                    //fine 
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }

                                }else errorFlag=1;
                            }
                            addr=intToHex(displacement);
                            addr=padded(addr,5,0,'0');
                            if(comma){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                            if(errorFlag){
                                string er="Undefined label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }else{
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                    }else if(terms.back()=="x"){//indexed mode +op m,X
                        ni[0]=ni[1]=1;
                        xbpe[3]=1;
                        if(address[currentSection].find(terms[1])!=address[currentSection].end()){
                            displacement=address[currentSection][terms[1]];
                            addr=intToHex(address[currentSection][terms[1]]);
                            addr=padded(addr,5,0,'0');
                        }else if(references[currentSection].count(terms[1])){
                            //fine
                        }else if(absoluteTerm(terms[1])){
                            displacement=stoi(terms[1]);
                        }else{
                            string er="Label not found in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);                       
                            continue;
                        }

                        string tmp;
                        for(int i=0;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }
                        if(terms[(int)terms.size()-2]!=","){
                            string er="Invalid format of instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        if(validExpressionType(tmp,currentSection)){
                            bool errorFlag=0;
                            int comma=0;
                            for(int i=2;i<(int)terms.size()-1;i++){
                                if(terms[i]=="-" || terms[i]=="+")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;                               
                                if(terms[i]==","){
                                    comma++;
                                    continue;
                                }
                                //label must be present in the csect or must be a reference
                                if(address[currentSection].count(terms[i])){
                                    //fine
                                    if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                    if(terms[i-1]=="-")displacement=displacement+(address[currentSection][terms[i]]);
                                }else if(references[currentSection].count(terms[i])){
                                    //fine 
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }

                                }else errorFlag=1;
                            }
                            addr=intToHex(displacement);
                            addr=padded(addr,5,0,'0');
                            if(comma!=1){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                            if(errorFlag){
                                string er="Undefined label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }else{
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                    }else{//+op m
                        ni[0]=ni[1]=1;
                        if(address[currentSection].find(terms[1])!=address[currentSection].end()){
                            displacement=address[currentSection][terms[1]];
                            addr=intToHex(address[currentSection][terms[1]]);
                            addr=padded(addr,5,0,'0');
                        }else if(references[currentSection].count(terms[1])){
                            displacement=0;
                            //fine
                        }else if(absoluteTerm(terms[1])){
                            displacement=stoi(terms[1]);
                        }else{
                            string er="Label not found in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);                       
                            continue;
                        }

                        string tmp;
                        for(int i=1;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }
                        
                        if(validExpressionUpdated(tmp,currentSection)){
                            bool errorFlag=0;
                            int comma=0;
                            for(int i=2;i<(int)terms.size();i++){
                                if(terms[i]=="-" || terms[i]=="+")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;                               
                                if(terms[i]==","){
                                    comma++;
                                    continue;
                                }
                                //label must be present in the csect or must be a reference
                                if(address[currentSection].count(terms[i])){
                                    //fine

                                    if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                    if(terms[i-1]=="-")displacement=displacement-(address[currentSection][terms[i]]);
                                }else if(references[currentSection].count(terms[i])){
                                    //fine 
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }

                                }else errorFlag=1;
                            }
                            addr=intToHex(displacement);
                            addr=padded(addr,5,0,'0');
                            if(comma){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                            if(errorFlag){
                                string er="Undefined label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }else{
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                    }
                    string op=intToHex(hexToInt(code[elements[0].substr(1)])+ni.to_ulong());
                    op=padded(op,2,0,'0');
                    string XBPE=intToHex(xbpe.to_ulong());
                    addr=intToHex(displacement);
                    addr=padded(addr,5,0,'0');
                    objectCode=objectCode+op+XBPE+addr;

                    modificationRecordType4(currentSection,terms,i);

                }else if(type1.find(first)!=type1.end()){
                    objectCode=objectCode+code[first];//ni default 11
                }else if(type2.find(first)!=type2.end()){
                    /*
                        shiftl r1,n
                        shiftr f1,n

                    clear r1
                    tixr r1
                        svc n

                    addr r1,r2
                    compr r1,r2
                    divr r1,r2
                    mulr r1,r2
                    rmo r1,r2
                    subr r1,r2
                    */

                    //remaining assembly: shiftl, shift, svc 
                    objectCode=objectCode+code[first];
                    if(first=="clear"){
                        objectCode=objectCode+registerCode[elements[1]];
                        objectCode=objectCode+"0";
                    }else if(first=="tixr"){
                        objectCode=objectCode+registerCode[elements[1]];
                        objectCode=objectCode+"0";
                    }else if(first=="svc"){
                        objectCode=objectCode+"00";
                    }else if(first=="shiftl"){
                        objectCode=objectCode+"00";
                    }else if(first=="shiftr"){
                        objectCode=objectCode+"00";
                    }else{
                        //two registers
                        objectCode=objectCode+registerCode[elements[1]]+registerCode[elements[2]];
                    }
                    
                }else if(type3.find(first)!=type3.end()){
                    //for type3: you can have a label in the operand field which must be defined in our 
                    // "address" map, if not then error. 
                    // we can also have indexed mode of addressing with a symbol.
                    // we can have immediate, indirect mode.
                    // if pc exceeds 12 bits, we need to do base.
                    // if even base exceeds 12 bits, it should be type 4, else error.
                    // n i x b p e, can't have indirect/immediate together with indexed.
                    // also gotta deal with literals
                    
                    if(first=="rsub"){
                        objectCode="4F0000";
                        objectCodes.push_back({"RSUB",objectCode});
                        continue;
                    }
                    bitset<2> ni;
                    bitset<4> xbpe;
                    bool canDoPC=1,canDoBase=0;
                    int displacement;
                    if(registerValue['b']!=-1)canDoBase=1;
                    if(terms[1][0]=='#'){
                        ni[0]=1;
                        if(absoluteTerm(terms[1].substr(1))){//lda #100
                            int value=stoi(terms[1].substr(1));
                            displacement=value;

                            int comma=0;
                            string tmp;
                            bool errorFlag=0;

                            for(int i=1;i<(int)terms.size();i++){
                                tmp=tmp+terms[i];
                                tmp=tmp+" ";
                            }

                            if(!validExpressionUpdated(tmp,currentSection)){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }

                            for(int i=2;i<(int)terms.size();i++){
                                if(terms[i]=="+" || terms[i]=="-")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;
                                if(terms[i]==","){
                                    comma++;
                                    break;
                                }
                                if(address[currentSection].count(terms[i])){
                                    if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                    if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }

                                }else{
                                    errorFlag=1; 
                                    break;
                                }
                            }

                            if(comma){
                                string er="Invalid instruction format: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                            }
                            if(errorFlag){
                                string er="Unknown label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }

                            if((int)terms.size()>2)displacement-=pc;

                            string inHex=intToHex(displacement);
                            //check that absolute value must fit in 3 hex bits
                            if((int)inHex.size()<=3){
                                displacement=value;
                            }else{
                                string er="Immediate value too large for 12 bits in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }else if(address[currentSection].count(terms[1].substr(1))){//lda #length
                            displacement=address[currentSection][terms[1].substr(1)]-pc;

                            int comma=0;
                            string tmp;
                            bool errorFlag=0;

                            for(int i=1;i<(int)terms.size();i++){
                                tmp=tmp+terms[i];
                                tmp=tmp+" ";
                            }

                            if(!validExpressionUpdated(tmp,currentSection)){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }

                            for(int i=2;i<(int)terms.size();i++){
                                if(terms[i]=="+" || terms[i]=="-")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;
                                if(terms[i]==","){
                                    comma++;
                                    break;
                                }
                                if(address[currentSection].count(terms[i])){
                                    if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                    if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }

                                }else{
                                    errorFlag=1; 
                                    break;
                                }
                            }

                            if(comma){
                                string er="Invalid instruction format: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                            }
                            if(errorFlag){
                                string er="Unknown label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }

                            string inHex=intToHex(displacement);
                            if((int)inHex.size()<=3){
                                //fine, can do pc
                                xbpe[1]=1;
                            }else{
                                //can't do pc
                                if(canDoBase){
                                    displacement+=pc;
                                    displacement=address[currentSection][terms[1].substr(1)]-registerValue['b'];
                                    string inHex=intToHex(displacement);
                                    if((int)inHex.size()<=3){
                                        xbpe[2]=1;
                                    }else{
                                        string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                        er=er+vec[i];
                                        er=er+"\"";
                                        errors.push_back(er);
                                        continue;
                                    }
                                }else{
                                    string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }
                        }else{//error
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        
                        string op=padded(intToHex(hexToInt(code[terms[0]])+ni.to_ulong()),2,0,'0');
                        string XBPE=intToHex(xbpe.to_ulong());
                        string disp=padded(intToHex(displacement),3,0,'0');
                        
                        objectCode=objectCode+op+XBPE+disp;

                    }else if(terms[1][0]=='@'){
                        ni[1]=1;
                        if(address[currentSection].count(terms[1].substr(1)) || absoluteTerm(terms[1].substr(1))){//lda @length
                            if(address[currentSection].count(terms[1].substr(1)))displacement=address[currentSection][terms[1].substr(1)]-pc;
                            else displacement=stoi(terms[1].substr(1))-pc;
                            int comma=0;
                            string tmp;
                            bool errorFlag=0;

                            for(int i=1;i<(int)terms.size();i++){
                                tmp=tmp+terms[i];
                                tmp=tmp+" ";
                            }

                            if(!validExpressionUpdated(tmp,currentSection)){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }

                            for(int i=2;i<(int)terms.size();i++){
                                if(terms[i]=="+" || terms[i]=="-")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;
                                if(terms[i]==","){
                                    comma++;
                                    break;
                                }
                                if(address[currentSection].count(terms[i])){
                                    if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                    if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }

                                }else{
                                    errorFlag=1; 
                                    break;
                                }
                            }

                            if(comma){
                                string er="Invalid instruction format: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                            }
                            if(errorFlag){
                                string er="Unknown label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }


                            string inHex=intToHex(displacement);
                            if((int)inHex.size()<=3){
                                //fine, can do pc
                                xbpe[1]=1;
                            }else{
                                //can't do pc
                                if(canDoBase){
                                    displacement+=pc;
                                    displacement=address[currentSection][terms[1].substr(1)]-registerValue['b'];
                                    string inHex=intToHex(displacement);
                                    if((int)inHex.size()<=3){
                                        xbpe[2]=1;
                                    }else{
                                        string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                        er=er+vec[i];
                                        er=er+"\"";
                                        errors.push_back(er);
                                        continue;
                                    }
                                }else{
                                    string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }
                        }else{//error
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        string op=padded(intToHex(hexToInt(code[terms[0]])+ni.to_ulong()),2,0,'0');
                        string XBPE=intToHex(xbpe.to_ulong());
                        string disp=padded(intToHex(displacement),3,0,'0');
                        
                        objectCode=objectCode+op+XBPE+disp;

                    }else if(terms.back()=="x"){//indexed mode, for e.x. stx length, x
                        ni[0]=ni[1]=1;
                        xbpe[3]=1;
                        if(address[currentSection].count(terms[1]) || absoluteTerm(terms[1])){//lda length
                            if(address[currentSection].count(terms[1])){
                                displacement=address[currentSection][terms[1]]-pc;
                            }else{
                                displacement=stoi(terms[1])-pc;
                            }

                            int comma=0;
                            string tmp;
                            bool errorFlag=0;

                            for(int i=0;i<(int)terms.size();i++){
                                tmp=tmp+terms[i];
                                tmp=tmp+" ";
                            }

                            if(!validExpressionType(tmp,currentSection)){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }

                            for(int i=2;i<(int)terms.size()-1;i++){
                                if(terms[i]=="+" || terms[i]=="-")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;
                                if(terms[i]==","){
                                    comma++;
                                    continue;
                                }
                                if(address[currentSection].count(terms[i])){
                                    if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                    if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }

                                }else{
                                    errorFlag=1; 
                                    break;
                                }
                            }
                            if(comma!=1 || terms[(int)terms.size()-2]!=","){
                                string er="Invalid instruction format: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                            }
                            if(errorFlag){
                                string er="Unknown label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }


                            string inHex=intToHex(displacement);
                            if((int)inHex.size()<=3){
                                //fine, can do pc
                                xbpe[1]=1;
                            }else{
                                //can't do pc
                                if(canDoBase){
                                    displacement+=pc;
                                    displacement=address[currentSection][terms[1]]-registerValue['b'];
                                    string inHex=intToHex(displacement);
                                    if((int)inHex.size()<=3){
                                        xbpe[2]=1;
                                    }else{
                                        string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                        er=er+vec[i];
                                        er=er+"\"";
                                        errors.push_back(er);
                                        continue;
                                    }
                                }else{
                                    string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }
                        }else{//error
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        string op=padded(intToHex(hexToInt(code[terms[0]])+ni.to_ulong()),2,0,'0');
                        string XBPE=intToHex(xbpe.to_ulong());
                        string disp=padded(intToHex(displacement),3,0,'0');
                        
                        objectCode=objectCode+op+XBPE+disp;

                    }else if(elements[1][0]=='='){//lda =x'f1'
                        ni[0]=ni[1]=1;
                        if(address[currentSection].count(elements[1])){//lda length
                            displacement=address[currentSection][elements[1]]-pc;
                            string inHex=intToHex(displacement);
                            if((int)inHex.size()<=3){
                                //fine, can do pc
                                xbpe[1]=1;
                            }else{
                                //can't do pc
                                if(canDoBase){
                                    displacement+=pc;
                                    displacement=address[currentSection][elements[1]]-registerValue['b'];
                                    string inHex=intToHex(displacement);
                                    if((int)inHex.size()<=3){
                                        xbpe[2]=1;
                                    }else{
                                        string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                        er=er+vec[i];
                                        er=er+"\"";
                                        errors.push_back(er);
                                        continue;
                                    }
                                }else{
                                    string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }
                        }else{//error
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        string op=padded(intToHex(hexToInt(code[elements[0]])+ni.to_ulong()),2,0,'0');
                        string XBPE=intToHex(xbpe.to_ulong());
                        string disp=padded(intToHex(displacement),3,0,'0');
                        
                        objectCode=objectCode+op+XBPE+disp;
                    }else{//normal: stx length
                        ni[0]=ni[1]=1;
                        if(address[currentSection].count(terms[1]) || absoluteTerm(terms[1])){//lda length
                            if(address[currentSection].count(terms[1])){
                                displacement=address[currentSection][terms[1]]-pc;
                            }else{
                                displacement=stoi(terms[1])-pc;
                            }

                            int comma=0;
                            string tmp;
                            bool errorFlag=0;

                            for(int i=1;i<(int)terms.size();i++){
                                tmp=tmp+terms[i];
                                tmp=tmp+" ";
                            }

                            if(!validExpressionUpdated(tmp,currentSection)){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }

                            for(int i=2;i<(int)terms.size();i++){
                                if(terms[i]=="+" || terms[i]=="-")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;
                                if(terms[i]==","){
                                    comma++;
                                    break;
                                }
                                if(address[currentSection].count(terms[i])){
                                    if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                    if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }

                                }else{
                                    errorFlag=1; 
                                    break;
                                }
                            }

                            if(comma || terms[(int)terms.size()-2]==","){
                                string er="Invalid instruction format: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                            }
                            if(errorFlag){
                                string er="Unknown label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }

                            string inHex=intToHex(displacement);
                            if((int)inHex.size()<=3){
                                //fine, can do pc
                                xbpe[1]=1;
                            }else{
                                //can't do pc
                                if(canDoBase){
                                    displacement+=pc;
                                    displacement=address[currentSection][elements[1]]-registerValue['b'];
                                    string inHex=intToHex(displacement);
                                    if((int)inHex.size()<=3){
                                        xbpe[2]=1;
                                    }else{
                                        string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                        er=er+vec[i];
                                        er=er+"\"";
                                        errors.push_back(er);
                                        continue;
                                    }
                                }else{
                                    string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }
                        }else{//error
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        string op=padded(intToHex(hexToInt(code[elements[0]])+ni.to_ulong()),2,0,'0');
                        string XBPE=intToHex(xbpe.to_ulong());
                        string disp=padded(intToHex(displacement),3,0,'0');
                        

                        objectCode=objectCode+op+XBPE+disp;

                    }


                }

            }else if(directives.find(first)!=directives.end()){
                if(first=="word"){
                    
                    terms.erase(terms.begin());//remove label

                    //word something.. remains

                    int displacement=0;
                    /*
                    word #bufend-buffer+100 incorrect
                    word #100-buffer+bufend incorrect
                    word @bufend-buffer+100 incorrect
                    word bufend-buffer+100 correct
                    */

                    string tmp;

                    for(int i=1;i<(int)terms.size();i++){
                        tmp=tmp+terms[i];
                        tmp=tmp+" ";
                    }

                    if(!validExpressionUpdated(tmp,currentSection)){
                        string er="Invalid expression in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                    int comma=0;
                    bool errorFlag=0;

                    for(int i=1;i<(int)terms.size();i++){
                        if(terms[i]=="+" || terms[i]=="-")continue;
                        if(terms[i]=="/" || terms[i]=="*")continue;
                        if(terms[i]==","){
                            comma++;
                            break;
                        }
                        if(absoluteTerm(terms[i])){
                            int tmp=stoi(terms[i]);
                            if(i+1<(int)terms.size()){
                                if(terms[i+1]=="/"){
                                    if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                        int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                        if(!v2){
                                            errorFlag=1;
                                            break;
                                        }
                                        tmp=v1/v2;
                                        if(i==1)displacement+=tmp;
                                        else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                        i+=2;
                                    }else{
                                        errorFlag=1;
                                        break;
                                    }
                                }else if(terms[i+1]=="*"){
                                    if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                        int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                        tmp=v1*v2;
                                        if(i==1){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                        i+=2;
                                    }else{
                                        errorFlag=1;
                                        break;
                                    }
                                }else{
                                    if(i==1){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }
                            }else{
                                if(i==1){
                                    displacement+=tmp;
                                }else{
                                    if(terms[i-1]=="+")displacement+=tmp;
                                    else if(terms[i-1]=="-")displacement-=tmp;
                                }
                            }

                        }else if(address[currentSection].count(terms[i])){
                            if(terms[i-1]=="+" || i==1)displacement+=address[currentSection][terms[i]];
                            if(terms[i-1]=="-")displacement-=address[currentSection][terms[i]];
                        }else if(references[currentSection].count(terms[i])){
                            //fine
                        }else errorFlag=1;
                    }
                    
                   
                    if(comma){
                        string er="Invalid instruction format: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                    }

                    if(errorFlag){
                        string er="Unknown label in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                    string inHex=intToHex(displacement);
                    if((int)inHex.size()>6){
                        string er="Value too large to be stored in 6 hex bits in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                    objectCode=objectCode+padded(intToHex(displacement),6,0,'0');

                    modificationRecordWord(currentSection,terms,i);

                }else if(first=="byte"){
                    if(elements[1][0]=='c'){
                        string tmp="";
                        for(int k=2;k<(int)elements[1].size()-1;k++){
                            char c=elements[1][k];
                            tmp=tmp+intToHex((int)(c));
                        }
                        objectCode=tmp;
                    }else if(elements[1][0]=='x'){
                        //valid length??
                        objectCode=elements[1].substr(2,(int)elements[1].size()-3);
                    }else{
                        string er="Invalid instruction Format for instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                }
            }else if(first=="equ"){
                //nothing to do here I guess
            }
        }else if(opCodes.find(first)!=opCodes.end()){
            //check that external references and other labels 
            //must be of the same control section
            // vector<string> terms=decomposeTerms(vec[i]);

            if(terms[0]=="+"){
                terms.erase(terms.begin());
                terms[0]='+'+terms[0];
            }
            if(first[0]=='+'){
                //modification record
                bitset<2> ni;
                bitset<4> xbpe;
                xbpe[0]=1;
                int displacement=0;
                string addr;
                if(terms[1][0]=='#'){//+op #m-a+b..
                    ni[0]=1;
                    if(absoluteTerm(terms[1].substr(1))){//lda #100-..+..
                        //check that absolute value must fit in 5 hex bits
                        int value=stoi(terms[1].substr(1));
                        string inHex=intToHex(value);
                        if((int)inHex.size()<=5){
                            displacement=value;
                            addr=intToHex(displacement);
                            addr=padded(addr,5,0,'0');
                        }else{
                            string er="Immediate value too large for 20 bits in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        //first term checked. move on to the remaining labels in the expression
                        string tmp;
                        for(int i=1;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }
                        if(terms.back()=="x"){
                            string er="Can't do indexed mode of addressing with immediate mode: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        if(validExpressionUpdated(tmp,currentSection)){
                            bool errorFlag=0;
                            int comma=0;
                            for(int i=2;i<(int)terms.size();i++){
                                if(terms[i]=="-" || terms[i]=="+")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;
                                if(terms[i]==","){
                                    comma++;
                                    break;
                                }
                                //label must be present in the csect or must be a reference
                                if(address[currentSection].count(terms[i])){
                                    //fine
                                    if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                    else if(terms[i-1]=="-")displacement=displacement-(address[currentSection][terms[i]]);
                                }else if(references[currentSection].count(terms[i])){
                                    //fine 
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }
                                }else errorFlag=1;
                            }
                            addr=intToHex(displacement);
                            addr=padded(addr,5,0,'0');
                            if(comma){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                            if(errorFlag){
                                string er="Undefined label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }else{
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                    }else if(address[currentSection].count(terms[1].substr(1))){
                        displacement=address[currentSection][terms[1].substr(1)];
                        addr=intToHex(address[currentSection][terms[1].substr(1)]);
                        addr=padded(addr,5,0,'0');
                        terms[1].erase(terms[1].begin());
                        string tmp;
                        for(int i=1;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }
                        if(terms.back()=="x"){
                            string er="Can't do indexed mode of addressing with immediate mode: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        if(validExpressionUpdated(tmp,currentSection)){
                            bool errorFlag=0;
                            int comma=0;
                            for(int i=2;i<(int)terms.size();i++){
                                if(terms[i]=="-" || terms[i]=="+")continue;
                                if(terms[i]=="/" || terms[i]=="*")continue;
                                if(terms[i]==","){
                                    comma++;
                                    continue;
                                }
                                //label must be present in the csect or must be a reference
                                if(address[currentSection].count(terms[i])){
                                    //fine
                                    if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                    else if(terms[i-1]=="-")displacement=displacement-(address[currentSection][terms[i]]);
                                }else if(references[currentSection].count(terms[i])){
                                    //fine 
                                }else if(absoluteTerm(terms[i])){
                                    int tmp=stoi(terms[i]);
                                    if(i+1<(int)terms.size()){
                                        if(terms[i+1]=="/"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                if(!v2){
                                                    errorFlag=1;
                                                    break;
                                                }
                                                tmp=v1/v2;
                                                if(i==2)displacement+=tmp;
                                                else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else if(terms[i+1]=="*"){
                                            if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                                int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                                tmp=v1*v2;
                                                if(i==2){
                                                    displacement+=tmp;
                                                }else{
                                                    if(terms[i-1]=="+")displacement+=tmp;
                                                    else if(terms[i-1]=="-")displacement-=tmp;
                                                }
                                                i+=2;
                                            }else{
                                                errorFlag=1;
                                                break;
                                            }
                                        }else{
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                        }
                                    }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }

                                }else errorFlag=1;
                            }
                            addr=intToHex(displacement);
                            addr=padded(addr,5,0,'0');
                            if(comma){
                                string er="Invalid expression in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                            if(errorFlag){
                                string er="Undefined label in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }else{
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                    }else if(references[currentSection].count(terms[1].substr(1))){
                        addr=padded(addr,5,0,'0');
                        displacement=0;
                        terms[1].erase(terms[1].begin());
                    }else{
                        string er="Label not found in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                }else if(terms[1][0]=='@'){//+op @m
                    ni[1]=1;
                    if(address[currentSection].count(terms[1].substr(1))){
                        //what to do here e.x. +jsub @wrrec
                        displacement=address[currentSection][terms[1].substr(1)];
                        addr=intToHex(address[currentSection][terms[1].substr(1)]);
                        addr=padded(addr,5,0,'0');
                        terms[1].erase(terms[1].begin());
                    }else if(references[currentSection].count(terms[1].substr(1))){
                        //fine
                    }else if(absoluteTerm(terms[1].substr(1))){
                        displacement=stoi(terms[1].substr(1));
                    }else{
                        string er="Label not found in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                    string tmp;
                    for(int i=1;i<(int)terms.size();i++){
                        tmp=tmp+terms[i];
                        tmp=tmp+" ";
                    }
                    if(terms.back()=="x"){
                        string er="Can't do indexed mode of addressing with indirect mode: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                    if(validExpressionUpdated(tmp,currentSection)){
                        bool errorFlag=0;
                        int comma=0;
                        for(int i=2;i<(int)terms.size();i++){
                            if(terms[i]=="-" || terms[i]=="+")continue;
                            if(terms[i]=="/" || terms[i]=="*")continue;
                            if(terms[i]==","){
                                comma++;
                                continue;
                            }
                            //label must be present in the csect or must be a reference
                            if(address[currentSection].count(terms[i])){
                                //fine
                                if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                if(terms[i-1]=="-")displacement=displacement+(address[currentSection][terms[i]]);
                            }else if(references[currentSection].count(terms[i])){
                                //fine 
                            }else if(absoluteTerm(terms[i])){
                                int tmp=stoi(terms[i]);
                                if(i+1<(int)terms.size()){
                                    if(terms[i+1]=="/"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            if(!v2){
                                                errorFlag=1;
                                                break;
                                            }
                                            tmp=v1/v2;
                                            if(i==2)displacement+=tmp;
                                            else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else if(terms[i+1]=="*"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            tmp=v1*v2;
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }
                                }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }

                            }else errorFlag=1;
                        }
                        addr=intToHex(displacement);
                        addr=padded(addr,5,0,'0');
                        if(comma){
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        if(errorFlag){
                            string er="Undefined label in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                    }else{
                        string er="Invalid expression in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                }else if(terms.back()=="x"){//indexed mode +op m,X
                    ni[0]=ni[1]=1;
                    xbpe[3]=1;
                    if(address[currentSection].find(terms[1])!=address[currentSection].end()){
                        displacement=address[currentSection][terms[1]];
                        addr=intToHex(address[currentSection][terms[1]]);
                        addr=padded(addr,5,0,'0');
                    }else if(references[currentSection].count(terms[1])){
                        //fine
                    }else if(absoluteTerm(terms[1])){
                        displacement=stoi(terms[1]);
                    }else{
                        string er="Label not found in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);                       
                        continue;
                    }

                    string tmp;
                    for(int i=0;i<(int)terms.size();i++){
                        tmp=tmp+terms[i];
                        tmp=tmp+" ";
                    }
                    if(terms[(int)terms.size()-2]!=","){
                        string er="Invalid format of instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                    if(validExpressionType(tmp,currentSection)){
                        bool errorFlag=0;
                        int comma=0;
                        for(int i=2;i<(int)terms.size()-1;i++){
                            if(terms[i]=="-" || terms[i]=="+")continue;
                            if(terms[i]=="/" || terms[i]=="*")continue;                               
                            if(terms[i]==","){
                                comma++;
                                continue;
                            }
                            //label must be present in the csect or must be a reference
                            if(address[currentSection].count(terms[i])){
                                //fine
                                if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                if(terms[i-1]=="-")displacement=displacement+(address[currentSection][terms[i]]);
                            }else if(references[currentSection].count(terms[i])){
                                //fine 
                            }else if(absoluteTerm(terms[i])){
                                int tmp=stoi(terms[i]);
                                if(i+1<(int)terms.size()){
                                    if(terms[i+1]=="/"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            if(!v2){
                                                errorFlag=1;
                                                break;
                                            }
                                            tmp=v1/v2;
                                            if(i==2)displacement+=tmp;
                                            else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else if(terms[i+1]=="*"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            tmp=v1*v2;
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }
                                }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }

                            }else errorFlag=1;
                        }
                        addr=intToHex(displacement);
                        addr=padded(addr,5,0,'0');
                        if(comma!=1){
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        if(errorFlag){
                            string er="Undefined label in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                    }else{
                        string er="Invalid expression in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                }else{//+op m
                    ni[0]=ni[1]=1;
                    if(address[currentSection].find(terms[1])!=address[currentSection].end()){
                        displacement=address[currentSection][terms[1]];
                        addr=intToHex(address[currentSection][terms[1]]);
                        addr=padded(addr,5,0,'0');
                    }else if(references[currentSection].count(terms[1])){
                        displacement=0;
                        //fine
                    }else if(absoluteTerm(terms[1])){
                        displacement=stoi(terms[1]);
                    }else{
                        string er="Label not found in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);                       
                        continue;
                    }

                    string tmp;
                    for(int i=1;i<(int)terms.size();i++){
                        tmp=tmp+terms[i];
                        tmp=tmp+" ";
                    }
                    
                    if(validExpressionUpdated(tmp,currentSection)){
                        bool errorFlag=0;
                        int comma=0;
                        for(int i=2;i<(int)terms.size();i++){
                            if(terms[i]=="-" || terms[i]=="+")continue;
                            if(terms[i]=="/" || terms[i]=="*")continue;                               
                            if(terms[i]==","){
                                comma++;
                                continue;
                            }
                            //label must be present in the csect or must be a reference
                            if(address[currentSection].count(terms[i])){
                                //fine

                                if(terms[i-1]=="+" || i==2)displacement=displacement+(address[currentSection][terms[i]]);
                                if(terms[i-1]=="-")displacement=displacement-(address[currentSection][terms[i]]);
                            }else if(references[currentSection].count(terms[i])){
                                //fine 
                            }else if(absoluteTerm(terms[i])){
                                int tmp=stoi(terms[i]);
                                if(i+1<(int)terms.size()){
                                    if(terms[i+1]=="/"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            if(!v2){
                                                errorFlag=1;
                                                break;
                                            }
                                            tmp=v1/v2;
                                            if(i==2)displacement+=tmp;
                                            else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else if(terms[i+1]=="*"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            tmp=v1*v2;
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }
                                }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }

                            }else errorFlag=1;
                        }
                        addr=intToHex(displacement);
                        addr=padded(addr,5,0,'0');
                        if(comma){
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                        if(errorFlag){
                            string er="Undefined label in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                    }else{
                        string er="Invalid expression in instruction: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                }
                string op=intToHex(hexToInt(code[elements[0].substr(1)])+ni.to_ulong());
                op=padded(op,2,0,'0');
                string XBPE=intToHex(xbpe.to_ulong());
                addr=intToHex(displacement);
                addr=padded(addr,5,0,'0');
                objectCode=objectCode+op+XBPE+addr;

                modificationRecordType4(currentSection,terms,i);

            }else if(type1.find(first)!=type1.end()){
                objectCode=objectCode+code[first];//ni default 11
            }else if(type2.find(first)!=type2.end()){
                /*
                    shiftl r1,n
                    shiftr f1,n

                clear r1
                tixr r1
                    svc n

                addr r1,r2
                compr r1,r2
                divr r1,r2
                mulr r1,r2
                rmo r1,r2
                subr r1,r2
                */

                //remaining assembly: shiftl, shift, svc 
                objectCode=objectCode+code[first];
                if(first=="clear"){
                    objectCode=objectCode+registerCode[elements[1]];
                    objectCode=objectCode+"0";
                }else if(first=="tixr"){
                    objectCode=objectCode+registerCode[elements[1]];
                    objectCode=objectCode+"0";
                }else if(first=="svc"){
                    objectCode=objectCode+"00";
                }else if(first=="shiftl"){
                    objectCode=objectCode+"00";
                }else if(first=="shiftr"){
                    objectCode=objectCode+"00";
                }else{
                    //two registers
                    objectCode=objectCode+registerCode[elements[1]]+registerCode[elements[2]];
                }
                
            }else if(type3.find(first)!=type3.end()){
                //for type3: you can have a label in the operand field which must be defined in our 
                // "address" map, if not then error. 
                // we can also have indexed mode of addressing with a symbol.
                // we can have immediate, indirect mode.
                // if pc exceeds 12 bits, we need to do base.
                // if even base exceeds 12 bits, it should be type 4, else error.
                // n i x b p e, can't have indirect/immediate together with indexed.
                // also gotta deal with literals

                if(first=="rsub"){
                    objectCode="4F0000";
                    objectCodes.push_back({"RSUB",objectCode});
                    continue;
                }
                bitset<2> ni;
                bitset<4> xbpe;
                bool canDoPC=1,canDoBase=0;
                int displacement;
                if(registerValue['b']!=-1)canDoBase=1;
                if(terms[1][0]=='#'){
                    ni[0]=1;
                    if(absoluteTerm(terms[1].substr(1))){//lda #100
                        int value=stoi(terms[1].substr(1));
                        displacement=value;

                        int comma=0;
                        string tmp;
                        bool errorFlag=0;

                        for(int i=1;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }

                        if(!validExpressionUpdated(tmp,currentSection)){
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        for(int i=2;i<(int)terms.size();i++){
                            if(terms[i]=="+" || terms[i]=="-")continue;
                            if(terms[i]=="/" || terms[i]=="*")continue;
                            if(terms[i]==","){
                                comma++;
                                break;
                            }
                            if(address[currentSection].count(terms[i])){
                                if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                            }else if(absoluteTerm(terms[i])){
                                int tmp=stoi(terms[i]);
                                if(i+1<(int)terms.size()){
                                    if(terms[i+1]=="/"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            if(!v2){
                                                errorFlag=1;
                                                break;
                                            }
                                            tmp=v1/v2;
                                            if(i==2)displacement+=tmp;
                                            else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else if(terms[i+1]=="*"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            tmp=v1*v2;
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }
                                }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }

                            }else{
                                errorFlag=1; 
                                break;
                            }
                        }

                        if(comma){
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                        }
                        if(errorFlag){
                            string er="Unknown label in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        if((int)terms.size()>2)displacement-=pc;

                        string inHex=intToHex(displacement);
                        //check that absolute value must fit in 3 hex bits
                        if((int)inHex.size()<=3){
                            displacement=value;
                        }else{
                            string er="Immediate value too large for 12 bits in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }
                    }else if(address[currentSection].count(terms[1].substr(1))){//lda #length
                        displacement=address[currentSection][terms[1].substr(1)]-pc;

                        int comma=0;
                        string tmp;
                        bool errorFlag=0;

                        for(int i=1;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }

                        if(!validExpressionUpdated(tmp,currentSection)){
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        for(int i=2;i<(int)terms.size();i++){
                            if(terms[i]=="+" || terms[i]=="-")continue;
                            if(terms[i]=="/" || terms[i]=="*")continue;
                            if(terms[i]==","){
                                comma++;
                                break;
                            }
                            if(address[currentSection].count(terms[i])){
                                if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                            }else if(absoluteTerm(terms[i])){
                                int tmp=stoi(terms[i]);
                                if(i+1<(int)terms.size()){
                                    if(terms[i+1]=="/"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            if(!v2){
                                                errorFlag=1;
                                                break;
                                            }
                                            tmp=v1/v2;
                                            if(i==2)displacement+=tmp;
                                            else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else if(terms[i+1]=="*"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            tmp=v1*v2;
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }
                                }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }

                            }else{
                                errorFlag=1; 
                                break;
                            }
                        }

                        if(comma){
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                        }
                        if(errorFlag){
                            string er="Unknown label in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        string inHex=intToHex(displacement);
                        if((int)inHex.size()<=3){
                            //fine, can do pc
                            xbpe[1]=1;
                        }else{
                            //can't do pc
                            if(canDoBase){
                                displacement+=pc;
                                displacement=address[currentSection][terms[1].substr(1)]-registerValue['b'];
                                string inHex=intToHex(displacement);
                                if((int)inHex.size()<=3){
                                    xbpe[2]=1;
                                }else{
                                    string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }else{
                                string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }
                    }else{//error
                        string er="Invalid instruction format: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                    
                    string op=padded(intToHex(hexToInt(code[terms[0]])+ni.to_ulong()),2,0,'0');
                    string XBPE=intToHex(xbpe.to_ulong());
                    string disp=padded(intToHex(displacement),3,0,'0');
                    
                    objectCode=objectCode+op+XBPE+disp;

                }else if(terms[1][0]=='@'){
                    ni[1]=1;
                    if(address[currentSection].count(terms[1].substr(1)) || absoluteTerm(terms[1].substr(1))){//lda @length
                        if(address[currentSection].count(terms[1].substr(1)))displacement=address[currentSection][terms[1].substr(1)]-pc;
                        else displacement=stoi(terms[1].substr(1))-pc;
                        int comma=0;
                        string tmp;
                        bool errorFlag=0;

                        for(int i=1;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }

                        if(!validExpressionUpdated(tmp,currentSection)){
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        for(int i=2;i<(int)terms.size();i++){
                            if(terms[i]=="+" || terms[i]=="-")continue;
                            if(terms[i]=="/" || terms[i]=="*")continue;
                            if(terms[i]==","){
                                comma++;
                                break;
                            }
                            if(address[currentSection].count(terms[i])){
                                if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                            }else if(absoluteTerm(terms[i])){
                                int tmp=stoi(terms[i]);
                                if(i+1<(int)terms.size()){
                                    if(terms[i+1]=="/"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            if(!v2){
                                                errorFlag=1;
                                                break;
                                            }
                                            tmp=v1/v2;
                                            if(i==2)displacement+=tmp;
                                            else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else if(terms[i+1]=="*"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            tmp=v1*v2;
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }
                                }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }

                            }else{
                                errorFlag=1; 
                                break;
                            }
                        }

                        if(comma){
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                        }
                        if(errorFlag){
                            string er="Unknown label in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }


                        string inHex=intToHex(displacement);
                        if((int)inHex.size()<=3){
                            //fine, can do pc
                            xbpe[1]=1;
                        }else{
                            //can't do pc
                            if(canDoBase){
                                displacement+=pc;
                                displacement=address[currentSection][terms[1].substr(1)]-registerValue['b'];
                                string inHex=intToHex(displacement);
                                if((int)inHex.size()<=3){
                                    xbpe[2]=1;
                                }else{
                                    string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }else{
                                string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }
                    }else{//error
                        string er="Invalid instruction format: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                    string op=padded(intToHex(hexToInt(code[terms[0]])+ni.to_ulong()),2,0,'0');
                    string XBPE=intToHex(xbpe.to_ulong());
                    string disp=padded(intToHex(displacement),3,0,'0');
                    
                    objectCode=objectCode+op+XBPE+disp;

                }else if(terms.back()=="x"){//indexed mode, for e.x. stx length, x
                    ni[0]=ni[1]=1;
                    xbpe[3]=1;
                    if(address[currentSection].count(terms[1]) || absoluteTerm(terms[1])){//lda length
                        if(address[currentSection].count(terms[1])){
                            displacement=address[currentSection][terms[1]]-pc;
                        }else{
                            displacement=stoi(terms[1])-pc;
                        }

                        int comma=0;
                        string tmp;
                        bool errorFlag=0;

                        for(int i=0;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }

                        if(!validExpressionType(tmp,currentSection)){
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        for(int i=2;i<(int)terms.size()-1;i++){
                            if(terms[i]=="+" || terms[i]=="-")continue;
                            if(terms[i]=="/" || terms[i]=="*")continue;
                            if(terms[i]==","){
                                comma++;
                                continue;
                            }
                            if(address[currentSection].count(terms[i])){
                                if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                            }else if(absoluteTerm(terms[i])){
                                int tmp=stoi(terms[i]);
                                if(i+1<(int)terms.size()){
                                    if(terms[i+1]=="/"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            if(!v2){
                                                errorFlag=1;
                                                break;
                                            }
                                            tmp=v1/v2;
                                            if(i==2)displacement+=tmp;
                                            else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else if(terms[i+1]=="*"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            tmp=v1*v2;
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }
                                }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }

                            }else{
                                errorFlag=1; 
                                break;
                            }
                        }
                        if(comma!=1 || terms[(int)terms.size()-2]!=","){
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                        }
                        if(errorFlag){
                            string er="Unknown label in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }


                        string inHex=intToHex(displacement);
                        if((int)inHex.size()<=3){
                            //fine, can do pc
                            xbpe[1]=1;
                        }else{
                            //can't do pc
                            if(canDoBase){
                                displacement+=pc;
                                displacement=address[currentSection][terms[1]]-registerValue['b'];
                                string inHex=intToHex(displacement);
                                if((int)inHex.size()<=3){
                                    xbpe[2]=1;
                                }else{
                                    string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }else{
                                string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }
                    }else{//error
                        string er="Invalid instruction format: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                    string op=padded(intToHex(hexToInt(code[terms[0]])+ni.to_ulong()),2,0,'0');
                    string XBPE=intToHex(xbpe.to_ulong());
                    string disp=padded(intToHex(displacement),3,0,'0');
                    
                    objectCode=objectCode+op+XBPE+disp;

                }else if(elements[1][0]=='='){//lda =x'f1'
                    ni[0]=ni[1]=1;
                    if(address[currentSection].count(elements[1])){//lda length
                        displacement=address[currentSection][elements[1]]-pc;
                        string inHex=intToHex(displacement);
                        if((int)inHex.size()<=3){
                            //fine, can do pc
                            xbpe[1]=1;
                        }else{
                            //can't do pc
                            if(canDoBase){
                                displacement+=pc;
                                displacement=address[currentSection][elements[1]]-registerValue['b'];
                                string inHex=intToHex(displacement);
                                if((int)inHex.size()<=3){
                                    xbpe[2]=1;
                                }else{
                                    string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }else{
                                string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }
                    }else{//error
                        string er="Invalid instruction format: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }

                    string op=padded(intToHex(hexToInt(code[elements[0]])+ni.to_ulong()),2,0,'0');
                    string XBPE=intToHex(xbpe.to_ulong());
                    string disp=padded(intToHex(displacement),3,0,'0');
                    
                    objectCode=objectCode+op+XBPE+disp;
                }else{//normal: stx length
                    ni[0]=ni[1]=1;
                    if(address[currentSection].count(terms[1]) || absoluteTerm(terms[1])){//lda length
                        if(address[currentSection].count(terms[1])){
                            displacement=address[currentSection][terms[1]]-pc;
                        }else{
                            displacement=stoi(terms[1])-pc;
                        }

                        int comma=0;
                        string tmp;
                        bool errorFlag=0;

                        for(int i=1;i<(int)terms.size();i++){
                            tmp=tmp+terms[i];
                            tmp=tmp+" ";
                        }

                        if(!validExpressionUpdated(tmp,currentSection)){
                            string er="Invalid expression in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        for(int i=2;i<(int)terms.size();i++){
                            if(terms[i]=="+" || terms[i]=="-")continue;
                            if(terms[i]=="/" || terms[i]=="*")continue;
                            if(terms[i]==","){
                                comma++;
                                break;
                            }
                            if(address[currentSection].count(terms[i])){
                                if(terms[i-1]=="+" || i==2)displacement=displacement+address[currentSection][terms[i]];
                                if(terms[i-1]=="-")displacement=displacement-address[currentSection][terms[i]];
                            }else if(absoluteTerm(terms[i])){
                                int tmp=stoi(terms[i]);
                                if(i+1<(int)terms.size()){
                                    if(terms[i+1]=="/"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            if(!v2){
                                                errorFlag=1;
                                                break;
                                            }
                                            tmp=v1/v2;
                                            if(i==2)displacement+=tmp;
                                            else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else if(terms[i+1]=="*"){
                                        if(i+2<(int)terms.size() && absoluteTerm(terms[i+2])){
                                            int v1=stoi(terms[i]), v2=stoi(terms[i+2]);
                                            tmp=v1*v2;
                                            if(i==2){
                                                displacement+=tmp;
                                            }else{
                                                if(terms[i-1]=="+")displacement+=tmp;
                                                else if(terms[i-1]=="-")displacement-=tmp;
                                            }
                                            i+=2;
                                        }else{
                                            errorFlag=1;
                                            break;
                                        }
                                    }else{
                                        if(i==2){
                                            displacement+=tmp;
                                        }else{
                                            if(terms[i-1]=="+")displacement+=tmp;
                                            else if(terms[i-1]=="-")displacement-=tmp;
                                        }
                                    }
                                }else{
                                    if(i==2){
                                        displacement+=tmp;
                                    }else{
                                        if(terms[i-1]=="+")displacement+=tmp;
                                        else if(terms[i-1]=="-")displacement-=tmp;
                                    }
                                }

                            }else{
                                errorFlag=1; 
                                break;
                            }
                        }

                        if(comma || terms[(int)terms.size()-2]==","){
                            string er="Invalid instruction format: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                        }
                        if(errorFlag){
                            string er="Unknown label in instruction: \"";
                            er=er+vec[i];
                            er=er+"\"";
                            errors.push_back(er);
                            continue;
                        }

                        string inHex=intToHex(displacement);
                        if((int)inHex.size()<=3){
                            //fine, can do pc
                            xbpe[1]=1;
                        }else{
                            //can't do pc
                            if(canDoBase){
                                displacement+=pc;
                                displacement=address[currentSection][elements[1]]-registerValue['b'];
                                string inHex=intToHex(displacement);
                                if((int)inHex.size()<=3){
                                    xbpe[2]=1;
                                }else{
                                    string er="Displacement exceeds limit for PC and Base Relative in instruction: \"";
                                    er=er+vec[i];
                                    er=er+"\"";
                                    errors.push_back(er);
                                    continue;
                                }
                            }else{
                                string er="Displacement exceeds limit for PC Relative in instruction: \"";
                                er=er+vec[i];
                                er=er+"\"";
                                errors.push_back(er);
                                continue;
                            }
                        }
                    }else{//error
                        string er="Invalid instruction format: \"";
                        er=er+vec[i];
                        er=er+"\"";
                        errors.push_back(er);
                        continue;
                    }
                    string op=padded(intToHex(hexToInt(code[elements[0]])+ni.to_ulong()),2,0,'0');
                    string XBPE=intToHex(xbpe.to_ulong());
                    string disp=padded(intToHex(displacement),3,0,'0');
                    
                    objectCode=objectCode+op+XBPE+disp;

                }

            }

        }else if(first=="base"){
            //can we have like base @length?? assuming no
            //nothing to do here
            if(elements[1][0]=='#'){//base #length
                string er="Invalid instruction format: \"";
                er=er+vec[i];
                er=er+"\"";
                errors.push_back(er);
            }else if(elements[1][0]=='@'){//base @length
                string er="Invalid instruction format: \"";
                er=er+vec[i];
                er=er+"\"";
                errors.push_back(er);
            }else{//base length
                registerValue['b']=address[currentSection][elements[1]];
            }
        }else if(first=="end"){
            string end="E"+padded(startingAddress,6,0,0);
        }else if(first=="nobase"){
            registerValue['b']=-1;
        }else{ 
        }
        objectCodes.push_back({vec[i],objectCode});
    }
}

void reorderRecords(){
    for(auto x: headerNames){
        vector<string> tmp;
        map<char,vector<string>> mp;

        for(auto y: records[x]){
            mp[y[0]].push_back(y);
        }
        for(auto x: mp['H'])tmp.push_back(x);
        for(auto x: mp['D'])tmp.push_back(x);
        for(auto x: mp['R'])tmp.push_back(x);
        for(auto x: mp['T'])tmp.push_back(x);
        for(auto x: mp['M'])tmp.push_back(x);
        bool pres=0;
        if((int)mp['E'].size()>1){
            pres=1;
        }
        bool pushed=0;
        for(auto x: mp['E']){
            if(pres && x=="E")continue;
            pushed=1;
            tmp.push_back(x);
        }
        if(!pushed)tmp.push_back("E");
        for(auto &x: tmp)x=upperCase(x);
        records[x]=tmp;
    }
}

void generateRecords(string firstExecutable){
    //break at resw, resb, csect, extdef, extref
    int start=0;
    vector<string> terms;
    string header;
    header=header+'H';
    string name;
    string progName;
    string txt;
    int i=0;
    while(i<(int)listing.size()){
        terms=decomposeTerms(objectCodes[i].first);
        if(!i){
            if((int)terms.size()==3){
                start=hexToInt(terms[2]);
            }
            name=terms[0];
            progName=terms[0];
            string tmp=terms[0];
            tmp=padded(tmp,6,1,'_');
            header=header+tmp;
            tmp="0";
            if((int)terms.size()>2){
                tmp=(terms[2]);
            }
            tmp=padded(tmp,6,0,'0');
            header=header+tmp;
            i++;
            continue;
        }
        if((int)terms.size()>1 && lowerCase(terms[1])=="csect"){
            string tmp=intToHex(sectionSize[name]);
            tmp=padded(tmp,6,0,'0');
            header=header+tmp;
            records[name].insert(records[name].begin(),header);
            header.clear();
            records[name].push_back("E");

            header=header+'H';
            tmp=terms[0];
            tmp=padded(tmp,6,1,'_');
            header=header+tmp;
            tmp=intToHex(0);
            tmp=padded(tmp,6,0,'0');
            header=header+tmp;
            
            name=terms[0];
            start=0;
            i++;
            continue;
        }
        if(lowerCase(terms[0])=="end"){
            string end;
            end=end+'E';
            string tmp=firstExecutable;
            tmp=padded(tmp,6,0,'0');
            end=end+tmp;

            records[progName].push_back(end);
            if(name!=progName)records[name].push_back("E");
            i++;
            continue;
        }
        if(lowerCase(terms[0])=="extdef"){
            string s;
            s=s+'D';
            for(int i=1;i<(int)terms.size();i++){
                if(address[name].count(terms[i])){
                    string tmp=terms[i];
                    tmp=padded(tmp,6,1,'_');
                    string tmp2=intToHex(address[name][terms[i]]);
                    tmp2=padded(tmp2,6,0,'0');
                    if((int)s.size()+12>74){
                        records[name].push_back(s);
                        s.clear();
                    }
                    s=s+tmp;
                    s=s+tmp2;
                }
            }
            if(!s.empty())records[name].push_back(s);
            i++;
            continue;
        }else if(lowerCase(terms[0])=="extref"){
            string s;
            s=s+'R';
            for(int i=1;i<(int)terms.size();i++){
                if(references[name].count(terms[i])){
                    string tmp=terms[i];
                    tmp=padded(tmp,6,1,'_');
                    if((int)s.size()+6>74){
                        records[name].push_back(s);
                        s.clear();
                    }
                    s=s+tmp;
                }
            }
            if(!s.empty())records[name].push_back(s);
            i++;
            continue;
        }
        if((int)terms.size()>1 && lowerCase(terms[1])=="equ" || lowerCase(terms[1])=="resw" || lowerCase(terms[1])=="resb"){
            i++;
            continue;
        }
        if(lowerCase(terms[0])=="base" || lowerCase(terms[0])=="nobase" || lowerCase(terms[0])=="ltorg" || lowerCase(terms[0])=="org"){
            i++;
            continue;
        }
        int txtsz=0;
        string s;
        s=s+'T';
        while(i<(int)listing.size()){
            terms=decomposeTerms(listing[i].second);
            if((int)terms.size()>1 && lowerCase(terms[1])=="resw" || lowerCase(terms[1])=="resb"){
                break;
            }
            if((int)terms.size()>1 && lowerCase(terms[1])=="equ"){
                i++;
                continue;
            }   
            if(lowerCase(terms[0])=="base" || lowerCase(terms[0])=="nobase" || lowerCase(terms[0])=="ltorg" || lowerCase(terms[0])=="org"){
                i++;
                continue;
            }
            if((int)terms.size()>1 && lowerCase(terms[1])=="csect"){
                int sz=(int)s.size()-9;
                sz/=2;
                string size=intToHex(sz);
                size=padded(size,2,0,'0');
                s[7]=size[0];
                s[8]=size[1];
                records[name].push_back(s);
                s.clear();
                i--;
                break;
            }
            if(lowerCase(terms[0])=="end"){
                string end;
                end=end+'E';
                string tmp=firstExecutable;
                tmp=padded(tmp,6,0,'0');
                end=end+tmp;
                records[progName].push_back(end);
                if(name!=progName)records[name].push_back("E");
                i++;
                continue;
            }
            if((int)s.size()==1){
                //starting address
                string tmp=listing[i].first;
                tmp=padded(tmp,6,0,'0');
                s=s+tmp;
                s=s+"**";//insert length of object code here later
                tmp=objectCodes[i].second;
                s=s+tmp;
            }else{
                if((int)s.size()+6>70){
                    int sz=(int)s.size()-9;
                    sz/=2;
                    string size=intToHex(sz);
                    size=padded(size,2,0,'0');
                    s[7]=size[0];
                    s[8]=size[1];
                    records[name].push_back(s);
                    s.clear();
                    i--;
                    break;
                }else{
                    string tmp=objectCodes[i].second;
                    s=s+tmp;
                }
            }
            i++;
        }
        if(!s.empty()){
            int sz=(int)s.size()-9;
            sz/=2;
            string size=intToHex(sz);
            size=padded(size,2,0,'0');
            s[7]=size[0];
            s[8]=size[1];
            records[name].push_back(s);
            s.clear();
        }
        i++;
    }
    string tmp=intToHex(sectionSize[name]);
    tmp=padded(tmp,6,0,'0');
    header=header+tmp;
    records[name].insert(records[name].begin(),header);
    auto it=find(records[progName].begin(),records[progName].end(),"E");
    reorderRecords();
}

int main(){
    init();
    vector<string> instructions=takeInstructions();
    
    pass1(instructions);

    if(!errors.empty()){//errors found
    
        cout<<"Unable to create listing file as the following errors were found:\n";
        for(auto x: errors){
            cout<<x<<'\n';
        }
    }else{//no errors found

        instructions.clear();
        for(auto x: listing){
            instructions.push_back(x.second);
        }
        
        pass2(instructions);

        if(!errors.empty()){
            cout<<"Unable to assign object codes as the following errors were found:\n";
            for(auto x: errors){
                cout<<x<<'\n';
            }
        }else{
            vector<string> noPrint={"base","nobase","ltorg","org","end","extref","extdef"};
            vector<string> elements;
            int mxPadding=0;
            for(int i=0;i<(int)listing.size();i++){
                mxPadding=max(mxPadding,(int)listing[i].first.size());
            }

            cout<<"\n\nListing File:\n\n";

            string firstExecutable;

            for(int i=0;i<(int)listing.size();i++){
                elements=decomposeElements(listing[i].second);
                if(find(noPrint.begin(),noPrint.end(),elements[0])!=noPrint.end()){
                    listing[i].first="";
                    cout<<listing[i].first<<setw(20-listing[i].first.size()-mxPadding)<<left;
                    cout<<"\t";

                    cout<<upperCase(listing[i].second)<<setw(60-(int)listing[i].second.size())<<right;

                    cout<<objectCodes[i].second<<'\n';
                    continue;
                }
                listing[i].first=padded(listing[i].first,mxPadding,0,'0');
                cout<<listing[i].first<<setw(20-(int)listing[i].first.size())<<left;
                cout<<"\t";

                cout<<upperCase(listing[i].second)<<setw(60-(int)listing[i].second.size())<<right;

                cout<<objectCodes[i].second<<'\n';
                if(firstExecutable.empty()){
                    firstExecutable=listing[i].first;
                }
            }

            generateRecords(firstExecutable);
            
            cout<<"\n\n\nObject Program: \n\n";

            for(auto name: headerNames){
                for(auto x: records[name]){
                    cout<<x<<'\n';
                }
                cout<<"\n\n";
            }

        }
    } 
    return 0;
}