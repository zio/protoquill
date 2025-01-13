package io.getquill.parser

import io.getquill.ast.{Ident => AIdent, Query => AQuery, _}

import scala.quoted._
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable

import scala.reflect.ClassTag
import io.getquill.norm.capture.AvoidAliasConflict
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.EntityQuery
import io.getquill.Query
import io.getquill.util.Format
import io.getquill.parser.ParserHelpers._
import io.getquill.quat.QuatMaking
import io.getquill.quat.Quat
import io.getquill.metaprog.Extractors._
import io.getquill.ast
import io.getquill.parser.engine._
import io.getquill.quat.QuatMakingBase
import io.getquill.norm.TranspileConfig

object ParserHelpers {

  trait Helpers(using Quotes) extends Idents with QuatMaking with QuatMakingBase

  trait Idents extends QuatMaking {
    def parseName(rawName: String) =
      // val name = rawName.replace("_$", "x").replace("$", "")
      // if (name.trim == "") "x" else name.trim
      rawName.replace("_$", "x")
    def cleanIdent(name: String, quat: Quat): AIdent =
      AIdent(parseName(name), quat)
    def cleanIdent(using Quotes)(name: String, tpe: quotes.reflect.TypeRepr): AIdent =
      AIdent(parseName(name), InferQuat.ofType(tpe))
  }

  trait Assignments extends Idents {

    import io.getquill.util.Interpolator
    import io.getquill.util.Messages.TraceType
    import io.getquill.norm.BetaReduction
    import io.getquill.metaprog.Extractors.ArrowFunction

    def rootParse: Parser

    object AssignmentTerm {
      object Components {
        def unapply(expr: Expr[_])(using Quotes) =
          UntypeExpr(expr) match {
            case Lambda1(ident, identTpe, ArrowFunction(prop, value)) => Some((ident, identTpe, prop, value))
            case _                                                    => None
          }
      }

      object TwoComponents {
        def unapply(expr: Expr[_])(using Quotes) =
          UntypeExpr(expr) match {
            case Lambda2(ident1, identTpe1, ident2, identTpe2, ArrowFunction(prop, value)) => Some((ident1, identTpe1, ident2, identTpe2, prop, value))
            case _                                                                         => None
          }
      }

      object CheckTypes {
        def checkPropAndValue(parent: Expr[Any], prop: Expr[Any], value: Expr[Any])(using Quotes) = {
          import quotes.reflect._
          val valueTpe = value.asTerm.tpe.widen
          val propTpe = prop.asTerm.tpe.widen
          // If both numbers are numeric and primitive e.g. `_.age -> 22.toShort` (in: `query[Person].insert(_.age -> 22.toShort)`)
          // then check if one can fit into another. If it can the assignment is valid
          if (isNumericPrimitive(propTpe) && isNumericPrimitive(valueTpe)) {
            if (!(numericPrimitiveFitsInto(propTpe, valueTpe))) {
              report.throwError(
                s"The primitive numeric value ${Format.TypeRepr(valueTpe)} in ${Format.Expr(value)} is to large to fit into the ${Format.TypeRepr(propTpe)} in ${Format.Expr(prop)}.",
                parent
              )
            }
          }
          // Otherwise check if the property is a subtype of the value that is being assigned to it
          else if (!(valueTpe <:< propTpe)) {
            report.throwError(
              s"The ${Format.TypeRepr(valueTpe)} value ${Format.Expr(value)} cannot be assigned to the ${Format.TypeRepr(propTpe)} property ${Format.Expr(prop)} because they are not the same type (or a subtype).",
              parent
            )
          }
        }
        def apply(expr: Expr[_])(using Quotes) = {
          import quotes.reflect._
          expr match {
            case Components(_, _, prop, value)          => checkPropAndValue(expr, prop, value)
            case TwoComponents(_, _, _, _, prop, value) => checkPropAndValue(expr, prop, value)
            case other =>
              report.throwError(s"The assignment statement ${Format.Expr(expr)} is invalid.")
          }
        }
      } // end CheckTypes

      def OrFail(expr: Expr[_])(using Quotes, History, LiftsAccum) =
        unapply(expr).getOrElse { failParse(expr, classOf[Assignment]) }

      def unapply(expr: Expr[_])(using Quotes, History, LiftsAccum): Option[Assignment] =
        UntypeExpr(expr) match {
          case Components(ident, identTpe, prop, value) =>
            Some(Assignment(cleanIdent(ident, identTpe), rootParse(prop), rootParse(value)))
          case _ => None
        }

      object Double {
        def OrFail(expr: Expr[_])(using Quotes, History, LiftsAccum) =
          unapply(expr).getOrElse { failParse(expr, classOf[AssignmentDual]) }
        def unapply(expr: Expr[_])(using Quotes, History, LiftsAccum): Option[AssignmentDual] =
          UntypeExpr(expr) match {
            case TwoComponents(ident1, identTpe1, ident2, identTpe2, prop, value) =>
              val i1 = cleanIdent(ident1, identTpe1)
              val i2 = cleanIdent(ident2, identTpe2)
              val valueAst = Transform(rootParse(value)) {
                case `i1` => OnConflict.Existing(i1)
                case `i2` => OnConflict.Excluded(i2)
              }
              Some(AssignmentDual(i1, i2, rootParse(prop), valueAst))
            case _ => None
          }
      }

    } // end AssignmentTerm
  } // end Assignments

  trait PropertyParser(implicit val qctx: Quotes) {
    import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
    import io.getquill.Embedded

    def rootParse: Parser

    // Parses (e:Entity) => e.foo (or e.foo.bar etc...)
    object LambdaToProperty {
      object OrFail {
        def apply(expr: Expr[_])(using History, LiftsAccum): Property =
          unapply(expr) match {
            case Some(value) => value
            case None =>
              report.throwError(s"Could not parse a (x) => x.property expression from: ${Format.Expr(expr)}", expr)
          }
      }

      def unapply(expr: Expr[_])(using History, LiftsAccum): Option[Property] =
        expr match {
          case Lambda1(id, tpe, body) =>
            val bodyProperty = AnyProperty.OrFail(body)
            // TODO Get the inner ident and verify that it's the same is 'id'
            Some(bodyProperty)
          case _ => None
        }
    } // end LambdaToProperty

    object AnyProperty {
      object OrFail {
        def apply(expr: Expr[_])(using History, LiftsAccum): Property =
          unapply(expr) match {
            case Some(value) => value
            case None =>
              report.throwError(s"Could not parse a ast.Property from the expression: ${Format.Expr(expr)}", expr)
          }
      }

      def unapply(expr: Expr[_])(using History, LiftsAccum): Option[Property] =
        expr match {
          case Unseal(value @ Select(Seal(prefix), member)) =>
            val propertyAst = Property(rootParse(prefix), member)
            // Generally when you have nested select properties (e.g. query[Person].map(p => p.name.first)) then you want to
            // select only the last thing i.e. `first`. Note that these kinds of nested selects can be in any clause of the query
            // including DistinctOn, OrderBy etc...
            // There are two cases when this happens
            // 1) When embedded case classes are used e.g. case class Person(name: Name), case class Name(first:String).
            //    In Scala2-Quill it was required that `Name` has an `extends Embedded` by in ProtoQuill this is not required.
            //    In this case in the SQL we just want to take the last property in the chain `p.name.first` i.e. `first`.
            // 2) When ad-hoc case classes are used in such as way as to form nested queries the names of the nested items
            //    are concatonated so that sub-select variables are unique.
            //    For example:
            //      (assuming: cc Contact(firstName:String), cc Person(name:Name, firstName:String), cc Name(firstName:String), note that firstName field is intentually redundant)
            //      query[Contact].nested.map(c => Person(Name(c.firstName), c.firstName)).nested needs to become:
            //      SELECT x.namefirstName AS firstName, x.firstName FROM (
            //        SELECT c.firstName AS namefirstName, c.firstName FROM ( -- Notice how Name becomes expanded to `namefirstName`, if Name has other properties e.g. Name.foo they become `namefoo`
            //          SELECT x.firstName FROM Contact x) c) x
            if (value.tpe <:< TypeRepr.of[Embedded] || propertyAst.quat.isProduct)
              Some(propertyAst.copyAll(visibility = Visibility.Hidden))
            else
              Some(propertyAst)
          case _ => None
        }
    } // end AnyProperty
  }

  trait PropertyAliases(using Quotes) {
    import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}

    import io.getquill.util.Interpolator
    import io.getquill.util.Messages.TraceType
    import io.getquill.norm.BetaReduction

    def rootParse: Parser

    object PropertyAliasExpr {
      def OrFail[T: Type](expr: Expr[Any]) = expr match {
        case PropertyAliasExpr(propAlias) => propAlias
        case _                            => failParse(expr, classOf[PropertyAlias])
      }

      def unapply[T: Type](expr: Expr[Any]): Option[PropertyAlias] =
        expr match {
          case Lambda1(_, _, '{ ($prop: Any).->[v](${ ConstExpr(alias: String) }) }) =>
            def path(tree: Expr[_]): List[String] =
              tree match {
                case a `.` b =>
                  path(a) :+ b
                case '{ (${ a `.` b }: Option[t]).map[r](${ Lambda1(arg, tpe, body) }) } =>
                  path(a) ++ (b :: path(body))
                case _ =>
                  Nil
              }
            Some(PropertyAlias(path(prop), alias))
          case _ =>
            None
        }
    }
  }

  /**
   * Helpers for different behaviors Quill supports of object equality. This is non-trivial since Quill has to make sense
   * of different equality paradigms across ANSI-SQL and Scala for objects that may be Optional or not. Several
   * techniques are implemented to resolve these inconsistencies.
   */
  trait ComparisonTechniques {

    // To be able to access the external parser the extends this
    def rootParse: Parser

    sealed trait EqualityBehavior { def operator: BinaryOperator }
    case object Equal extends EqualityBehavior { def operator: BinaryOperator = EqualityOperator.`_==` }
    case object NotEqual extends EqualityBehavior { def operator: BinaryOperator = EqualityOperator.`_!=` }

    /**
     * Taken from the identically named method in Parser.scala in Scala2-Quill. Much of this logic
     * is not macro specific so a good deal of it can be refactored out into the quill-sql-portable module.
     * Do equality checking on the database level with the same truth-table as idiomatic scala
     */
    def equalityWithInnerTypechecksIdiomatic(using Quotes, History, LiftsAccum)(left: quotes.reflect.Term, right: quotes.reflect.Term)(equalityBehavior: EqualityBehavior) = {
      import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
      import io.getquill.ast.Implicits._
      val (leftIsOptional, rightIsOptional) = checkInnerTypes(left, right, ForbidInnerCompare)
      val a = rootParse(left.asExpr)
      val b = rootParse(right.asExpr)
      val comparison = BinaryOperation(a, equalityBehavior.operator, b)
      (leftIsOptional, rightIsOptional, equalityBehavior) match {
        // == two optional things. Either they are both null or they are both defined and the same
        case (true, true, Equal) => (OptionIsEmpty(a) +&&+ OptionIsEmpty(b)) +||+ (OptionIsDefined(a) +&&+ OptionIsDefined(b) +&&+ comparison)
        // != two optional things. Either one is null and the other isn't. Or they are both defined and have different values
        case (true, true, NotEqual) => (OptionIsDefined(a) +&&+ OptionIsEmpty(b)) +||+ (OptionIsEmpty(a) +&&+ OptionIsDefined(b)) +||+ comparison
        // No additional logic when both sides are defined
        case (false, false, _) => comparison
        // Comparing an optional object with a non-optional object is not allowed when using scala-idiomatic optional behavior
        case (lop, rop, _) =>
          val lopString = (if (lop) "Optional" else "Non-Optional") + s" ${left}}"
          val ropString = (if (rop) "Optional" else "Non-Optional") + s" ${right}}"
          report.throwError(s"Cannot compare ${lopString} with ${ropString} using operator ${equalityBehavior.operator}", left.asExpr)
      }
    }

    /**
     * (not used yet but will be used when support for 'extras' dsl functionality is added)
     * Do equality checking on the database level with the ansi-style truth table (i.e. always false if one side is null)
     */
    def equalityWithInnerTypechecksAnsi(using Quotes, History, LiftsAccum)(left: quotes.reflect.Term, right: quotes.reflect.Term)(equalityBehavior: EqualityBehavior) = {
      import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
      import io.getquill.ast.Implicits._
      val (leftIsOptional, rightIsOptional) = checkInnerTypes(left, right, AllowInnerCompare)
      val a = rootParse(left.asExpr)
      val b = rootParse(right.asExpr)
      val comparison = BinaryOperation(a, equalityBehavior.operator, b)
      (leftIsOptional, rightIsOptional) match {
        case (true, true)   => OptionIsDefined(a) +&&+ OptionIsDefined(b) +&&+ comparison
        case (true, false)  => OptionIsDefined(a) +&&+ comparison
        case (false, true)  => OptionIsDefined(b) +&&+ comparison
        case (false, false) => comparison
      }
    }

    trait OptionCheckBehavior

    /** Allow T == Option[T] comparison * */
    case object AllowInnerCompare extends OptionCheckBehavior

    /** Forbid T == Option[T] comparison * */
    case object ForbidInnerCompare extends OptionCheckBehavior

    /**
     * Type-check two trees, if one of them has optionals, go into the optionals to find the root types
     * in each of them. Then compare the types that are inside. If they are not compareable, abort the build.
     * Otherwise return type of which side (or both) has the optional. In order to do the actual comparison,
     * the 'weak conformance' operator is used and a subclass is allowed on either side of the `==`. Weak
     * conformance is necessary so that Longs can be compared to Ints etc...
     */
    def checkInnerTypes(using Quotes)(lhs: quotes.reflect.Term, rhs: quotes.reflect.Term, optionCheckBehavior: OptionCheckBehavior): (Boolean, Boolean) = {
      import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
      val leftType = lhs.tpe
      val rightType = rhs.tpe
      // Note that this only goes inside the optional one level i.e. Option[T] => T. If we have Option[Option[T]] it will return the inside Option[T].
      // This is by design. If the types do not match, even if we normally don't care about the outer layer
      // (i.e. for equalityWithInnerTypechecksAnsi where Option[T] == T is allowed (that's in the 'extras' modules which uses ===))
      // we still want to fail with an exception that the types are identical if the user does Option[Option[T]] == Option[T] since that is a serious
      // typing error.
      val leftInner = innerOptionParam(leftType)
      val rightInner = innerOptionParam(rightType)
      val leftIsOptional = isOptionType(leftType) && !(leftType.widen =:= TypeRepr.of[Nothing]) && !(leftType.widen =:= TypeRepr.of[Null])
      val rightIsOptional = isOptionType(rightType) && !(rightType.widen =:= TypeRepr.of[Nothing]) && !(rightType.widen =:= TypeRepr.of[Null])
      val typesMatch = wideMatchTypes(rightInner, leftInner)

      optionCheckBehavior match {
        case AllowInnerCompare if typesMatch =>
          (leftIsOptional, rightIsOptional)
        case ForbidInnerCompare if ((leftIsOptional && rightIsOptional) || (!leftIsOptional && !rightIsOptional)) && typesMatch =>
          (leftIsOptional, rightIsOptional)
        case _ =>
          if (leftIsOptional || rightIsOptional)
            report.throwError(
              s"${Format.TypeReprW(leftType)} == ${Format.TypeReprW(rightType)} is not allowed since ${Format.TypeReprW(leftInner)}, ${Format.TypeReprW(rightInner)} are different types.",
              lhs.asExpr
            )
          else
            report.throwError(s"${Format.TypeReprW(leftType)} == ${Format.TypeReprW(rightType)} is not allowed since they are different types.", lhs.asExpr)
      }

    } // end checkInnerTypes

    def isOptionType(using Quotes)(tpe: quotes.reflect.TypeRepr) = {
      import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
      tpe <:< TypeRepr.of[Option[_]]
    }

    /**
     * Match types in the most wide way possible. This function is not for generalized type equality since quill does not directly
     * compare anything, rather it just translates things into SQL expressions. This kind of check is used in a general sense when things
     * that it doesn't even make sense to compare are compared e.g. an Person and a String. In this case, we want to provide some kind
     * of compile-time warning that the comparision the user is attempting to do in SQL is non sensical in the first place. Therefore when
     * there is any kind of possibility that the expression makes sense (e.g. by comparing a Dog to a Animal (i.e. class to subclass), by comparing
     * two numeric types of any kind etc... we allow the comparison to happen).
     * For int/long/float/double comparisons don't crash on compile-time typing can re-evaluate this upon user feedback
     */
    def wideMatchTypes(using Quotes)(a: quotes.reflect.TypeRepr, b: quotes.reflect.TypeRepr) =
      a.widen =:= b.widen || a.widen <:< b.widen || b.widen <:< a.widen || (isNumeric(a.widen) && isNumeric(b.widen))

    def innerOptionParam(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
      import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
      if (tpe <:< TypeRepr.of[Option[_]])
        tpe.asType match {
          case '[Option[t]] => TypeRepr.of[t]
        }
      else
        tpe
    }
  } // end ComparisonTechniques

  trait PatternMatchingValues extends Parser with QuatMaking {
    import io.getquill.util.Interpolator
    import io.getquill.util.Messages.TraceType
    import io.getquill.norm.BetaReduction

    def rootParse: Parser

    // don't change to ValDef or might override the real valdef in qctx.reflect
    object ValDefTerm {
      def unapply(using Quotes, History, LiftsAccum, TranspileConfig)(tree: quotes.reflect.Tree): Option[Ast] = {
        import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
        tree match {
          case TValDef(name, tpe, Some(t @ PatMatchTerm.SimpleClause(ast))) =>
            Some(Val(AIdent(name, InferQuat.ofType(tpe.tpe)), ast))

          // In case a user does a 'def' instead of a 'val' with no paras and no types then treat it as a val def
          // this is useful for things like (TODO Get name) where you'll have something like:
          // query[Person].map(p => (p.name, p.age)).filter(tup => tup._1.name == "Joe")
          // But in Scala3 you can do:
          // query[Person].map(p => (p.name, p.age)).filter((name, age) => name == "Joe")
          // Then in the AST it will look something like:
          // query[Person].map(p => (p.name, p.age)).filter(x$1 => { val name=x$1._1; val age=x$1._2; name == "Joe" })
          // and you need to resolve the val defs thare are created automatically
          case DefDef(name, paramss, tpe, rhsOpt) if (paramss.length == 0) =>
            // println(s"====== Parsing Def Def ${name} = ${rhsOpt.map(_.show)}")
            val body =
              rhsOpt match {
                // TODO Better site-description in error
                case None      => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(tree)}")
                case Some(rhs) => rhs
              }
            val bodyAst = rootParse(body.asExpr)
            Some(Val(AIdent(name, InferQuat.ofType(tpe.tpe)), bodyAst))

          case TValDef(name, tpe, rhsOpt) =>
            val body =
              rhsOpt match {
                // TODO Better site-description in error
                case None      => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(tree)}")
                case Some(rhs) => rhs
              }
            val bodyAst = rootParse(body.asExpr)
            Some(Val(AIdent(name, InferQuat.ofType(tpe.tpe)), bodyAst))

          case _ => None
        }
      }
    }

    case class PatMatchClause(body: Ast, guard: Ast)
    enum PatMatch {
      // Represents a variable assignment pattern match i.e. single clause with no guards e.g.
      // ptups.map { case (name, age) => ... } where ptups := people.map(p => (p.name, p.age))
      case SimpleClause(body: Ast) extends PatMatch
      case MultiClause(clauses: List[PatMatchClause]) extends PatMatch
      // In some cases, scala compiler adds a trivial boolean clause to a tuple pattern match
      // we detect these and can just spliced TRUE or 1=1 in those cases
      case AutoAddedTrivialClause
    }

    object PatMatchTerm {
      object SimpleClause {
        def unapply(using Quotes, History, LiftsAccum, TranspileConfig)(term: quotes.reflect.Term): Option[Ast] =
          PatMatchTerm.unapply(term) match {
            case Some(PatMatch.SimpleClause(ast)) => Some(ast)
            case _                                => None
          }
      }

      def unapply(using Quotes, History, LiftsAccum, TranspileConfig)(root: quotes.reflect.Term): Option[PatMatch] = {
        import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
        root match {
          case Match(expr, List(CaseDef(fields, None, body))) =>
            Some(PatMatch.SimpleClause(betaReduceTupleFields(expr, fields)(body)))

          case Match(
                expr,
                List(
                  CaseDef(fields, None, Literal(BooleanConstant(true))),
                  CaseDef(TIdent("_"), None, Literal(BooleanConstant(false)))
                )
              ) =>
            Some(PatMatch.AutoAddedTrivialClause)

          case m @ Match(expr, caseDefs) =>
            println(s"Doing Multi-Clause Pat-match: ${Format(Printer.TreeStructure.show(m))}")
            val clauses =
              caseDefs.map {
                case CaseDef(fields, guard, body) =>
                  val bodyAst = betaReduceTupleFields(expr, fields, Some(root))(body)
                  val guardAst = guard.map(betaReduceTupleFields(expr, fields)(_)).getOrElse(ast.Constant(true, Quat.BooleanValue))
                  PatMatchClause(bodyAst, guardAst)
              }
            Some(PatMatch.MultiClause(clauses))

          case other => None
        }
      } // end unapply
    }

    /**
     * Beta-reduces out tuple members that have been pattern matched to their corresponding components
     * For example:
     * given: ptups := people.map(p => (p.name, p.age))
     * ptups.map { case (name, age) => fun(name, age) }
     * becomes reduced to:
     * ptups.map { x => fun(x.name, x.age) }
     */
    protected def betaReduceTupleFields(using Quotes, History, LiftsAccum, TranspileConfig)(tupleTree: quotes.reflect.Term, fieldsTree: quotes.reflect.Tree, messageExpr: Option[quotes.reflect.Term] = None)(bodyTree: quotes.reflect.Term): Ast = {
      import quotes.reflect.{Ident => TIdent, ValDef => TValDef, _}
      // TODO Need to verify that this is actually a tuple?
      val tuple = rootParse(tupleTree.asExpr)
      val bodyRaw = rootParse(bodyTree.asExpr)
      // In some cases the body expression itself is so complex it needs to be beta-reduced before we start
      // beta reducing the pat match tuples otherwise some issues can happen. This was discovered in the DepartmentsSpec tests
      val body = BetaReduction(bodyRaw)

      /*
      Get a list of all the paths of all the identifiers inside the tuple. E.g:
      foo match { case ((a,b),c) => bar } would yield something like:
      List((a,List(_1, _1)), (b,List(_1, _2)), (c,List(_2)))
       */
      def tupleBindsPath(field: Tree, path: List[String] = List()): List[(AIdent, List[String])] = {
        UntypeTree(field) match {
          case Bind(name, TIdent(_)) => List(AIdent(name) -> path)
          case Unapply(Method0(TupleIdent(), "unapply"), something, binds) =>
            binds.zipWithIndex.flatMap { case (bind, idx) =>
              tupleBindsPath(bind, path :+ s"_${idx + 1}")
            }
          case Unapply(TypeApply(Select(TIdent("*:"), "unapply"), types), implicits, List(h, t)) =>
            List(
              tupleBindsPath(h, path :+ s"head"),
              tupleBindsPath(h, path :+ s"tail")
            )
              .flatten
          // If it's a "case _ => ..." then that just translates into the body expression so we don't
          // need a clause to beta reduction over the entire partial-function
          case TIdent("_") =>
            List()
          case other =>
            val addition =
              messageExpr match {
                case Some(expr) => s" in the expression: ${Format.Tree(expr)}"
                case None       => ""
              }
            report.throwError(s"Invalid Pattern Matching Term: ${Format.Tree(other)}${addition}.\n" +
              s"Quill Query Pattern matches must be correctly matching tuples.\n" +
              s"For example for query[Person].map(p => (p.name, p.age)) you can then do:\n" +
              s"query[Person].map(p => (p.name, p.age)).map { case (name, age) => ... }")
        }
      }

      /* Take the list found in the tupleBindsPath method above and match up each match-tuple element
      from the original tuple we found. For example, if we had: foo match { case ((a,b),c) => bar }
      we get something like List((a,List(_1, _1)), (b,List(_1, _2)), (c,List(_2))). If 'foo'
      is ((f,b),z) then we want to get: List(((f,b),z)._1._1, ((f,b),z)._1._2, ((f,b),z)._2)
       */
      def propertyAt(path: List[String]) =
        path.foldLeft(tuple) {
          case (tup, elem) => Property(tup, elem)
        }

      val fieldPaths = tupleBindsPath(fieldsTree)
      val reductionTuples = fieldPaths.map((id, path) => (id, propertyAt(path)))

      val interp = new Interpolator(TraceType.Standard, summon[TranspileConfig].traceConfig, 1)
      import interp._

      trace"Pat Match Parsing: ${body}".andLog()
      trace"Reductions: ${reductionTuples}".andLog()
      // Do not care about types here because pat-match body does not necessarily have correct typing in the Parsing phase
      val result = BetaReduction(body, io.getquill.norm.TypeBehavior.ReplaceWithReduction, reductionTuples: _*)
      trace"Result: ${result}".andLog()
      result
    }
  } // end PatternMatchingValues

  object ImplicitClassExtensionPattern {
    private def isImplicitClassMaker(using Quotes)(term: quotes.reflect.Term): Boolean = {
      import quotes.reflect._
      term.tpe.typeSymbol.flags.is(Flags.Implicit) && term.tpe.classSymbol.isDefined
    }

    private def isImplicitClassMethod(using Quotes)(term: quotes.reflect.Term): Boolean = {
      import quotes.reflect._
      term.tpe.termSymbol.flags.is(Flags.Final | Flags.Implicit | Flags.Method)
    }

    def unapply(expr: Expr[_])(using Quotes) = {
      import quotes.reflect._
      expr match {
        // Putting a type-apply in all possible places to detect all possible variations of the
        // implicit class pattern that we need to warn about for Scala 3 (since it no-longer works).
        case expr @ Unseal(UntypeApply(cc @ Apply(UntypeApply(ccid), List(constructorArg)))) if (isImplicitClassMaker(cc) && isImplicitClassMethod(ccid)) =>
          Some((ccid.tpe, constructorArg))
        case _ =>
          None
      }
    }
    def errorMessage(using Quotes)(expr: Expr[_], ccid: quotes.reflect.TypeRepr, constructorArg: quotes.reflect.Term) =
      s"""|Error in the expression:
          |  ${Format.Expr(expr)}
          |
          |Attempted to use an implicit extensions class `${ccid}`.
          |Implicit extensions in Quotations are not supported in ProtoQuill, they can
          |only be used with dynamic queries. Instead of implicit-classes, use inline
          |extension methods. For example, instead of doing this:
          |implicit class ${ccid}(input: ${Format.TypeRepr(constructorArg.tpe.widen)}):
          |  def myMethod = [method content]
          |
          |Do this:
          |extension (inline input: ${Format.TypeRepr(constructorArg.tpe.widen)})
          |  inline def myMethod = [method content]
          |"""".stripMargin
  }

} // end ParserHelpers
