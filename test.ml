module type VAL =
	sig
		type t
		val add : t -> t -> t
		val mul : t -> t -> t
	end;;

module type EVALEXPR =
	sig
		type t
		type expr =
			| Add of (expr * expr)
			| Mul of (expr * expr)
			| Value of t
		val eval : expr -> t
	end;;

module type METAEVALEXPR = functor (Val : VAL) -> EVALEXPR with type t = Val.t;;

module IntVal =
	struct
		type t = int
		let add a b = a + b
		let mul a b = a * b
	end;;

module FloatVal =
	struct
		type t = float
		let add a b = a +. b
		let mul a b = a *. b
	end;;

module MetaEvalExpr : METAEVALEXPR = functor (Val : VAL) ->
	struct
		type t = Val.t
		type expr =
			| Add of (expr * expr)
			| Mul of (expr * expr)
			| Value of Val.t

		let rec eval = function
			| Add (lhs, rhs)	-> Val.add (eval lhs) (eval rhs)
			| Mul (lhs, rhs)	-> Val.mul (eval lhs) (eval rhs)
			| Value v			-> v
	end;;

module IntEvalExpr = MetaEvalExpr (IntVal);;
module FloatEvalExpr = MetaEvalExpr (FloatVal);;

IntEvalExpr.eval (IntEvalExpr.Add
(IntEvalExpr.Value 40, IntEvalExpr.Value 2))
