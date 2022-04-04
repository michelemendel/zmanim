(ns groovy-parser
  (:import
   (org.codehaus.groovy.control CompilePhase)
   (org.codehaus.groovy.ast GroovyCodeVisitor)
   (org.codehaus.groovy.ast.builder AstBuilder)))

;; Example scripts
(def s1 "sum = 3*5+77")
(def s3 "
class Student {
   int StudentID;
   String StudentName;
   static void main(String[] args) {
         Student st = new Student();
         st.StudentID = 1;
         st.StudentName = \"Joe\"
   }
}
new Socket(myfn(9));
")
(def s4 "import org.apache.commons.io.FileUtils;")
(def s5 "def aaa = 12345;
         def xxxx = addd(345);
         yyy = kalleanka(add(3));
         adderall(2,f(3));
         Files.readAllBytes(Paths.get());
         g.V().hasLabel('Unrelated component')
         Student st = new Student();
         new Socket();
         newProcessBuilder();
         Runtime.getRuntime().exec()
         System.currentTimeSeconds()
         ")
(def s6 "
def additionOfxAndY(x,y) {return x+y};
zzz = additionOfxAndY(2,3)
x = addddd(2,3)
kalleanka(3333 + x);")
(def s7 "g.V().hasLabel('Unrelated component').as('unrelated')
           .outE().as(\"firstref\").values('active').as('activeref1')
           .select(\"firstref\").otherV().as('parent')
           .select('parent').outE().hasLabel('Implicit').as('childref')
           .values('active').as('activeref2')
           // The magic comparison
           .select('childref').where(\"activeref1\", eq('activeref2')).otherV().as('child')
           .select('unrelated', 'activeref1','parent', 'activeref2', 'child')
           .by('name').by().by('name').by().by('name')")

;;http://docs.groovy-lang.org/docs/groovy-2.1.3/html/gapi/org/codehaus/groovy/ast/builder/AstBuilder.html
;;https://docs.groovy-lang.org/latest/html/api/org/codehaus/groovy/ast/GroovyCodeVisitor.html
;;https://stackoverflow.com/questions/40431213/how-to-traverse-ast-tree
;;http://docs.groovy-lang.org/2.4.7/html/api/org/codehaus/groovy/antlr/GroovySourceAST.html

(defn- p [rule v]
  (println "-->" rule ":" v))

(def res (atom []))

(defn- acc
  [c e]
  (swap! res conj {c e}))

(defn visit [x v]
  ;;(println "VISIT" x)
  (.visit x v))

(def v
  (proxy [GroovyCodeVisitor] []
    (visitArgumentlistExpression [x]
      (mapv (fn [expr]
              ;;(acc :Argumentlist (.getText x))
              (visit expr v))
            (.getExpressions x)))
    (visitBinaryExpression [x]
      #_(acc :BinaryExpression [(.getText (.getLeftExpression x))
                              (.getText (.getOperation x))
                              (.getText (.getRightExpression x))])
      (visit (.getLeftExpression x) v)
      (visit (.getRightExpression x) v))
    (visitBlockStatement [x]
      (mapv (fn [child]
              ;;(acc :BlockStatement (.getText x))
              (visit child v))
            (.getStatements x)))
    (visitClassExpression [x]
      (acc :Class (.getValue x)))
    (visitConstantExpression [x]
     #_(acc :Constant (.getValue x)))
    (visitConstructorCallExpression [x]
      (acc :ConstructorCall (.getText x))
      (visit (.getArguments x) v))
    (visitDeclarationExpression [x]
      #_(acc :Declaration [(.getText (.getLeftExpression x))
                         (.getText (.getOperation x))
                         (.getText (.getRightExpression x))])
      (visit (.getLeftExpression x) v)
      (visit (.getRightExpression x) v))
    (visitExpressionStatement [x]
      ;;(acc :ExpressionStatement nil)
      (visit (.getExpression x) v))
    (visitFieldExpression [x]
      (acc :Field nil))
    (visitIfElse [x]
      (acc :IfElse nil)
      (mapv (fn [statement]
              (visit statement v))
            (.getStatements (.getIfBlock x)))
      (visit (.getElseBlock x) v))
    (visitMethodCallExpression [x]
      (acc :MethodCall (.getText x))
      (visit (.getMethod x) v)
      (visit (.getArguments x) v))
    (visitMethodPointerExpression [x]
      (acc :MethodPointer nil))
    (visitPropertyExpression [x]
      (acc :Property nil))
    (visitReturnStatement [x]
     (acc :ReturnStatement (.getText x)))
    (visitVariableExpression [x]
     #_(acc :Variable (.getText x)))))

(defn- traverse
  [script]
  (let [ast (.buildFromString (AstBuilder.) CompilePhase/CONVERSION script)]
    (visit (first ast) v)))

(traverse s5)

(clojure.pprint/pprint @res)





