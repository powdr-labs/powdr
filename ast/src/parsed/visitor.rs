use std::{iter, ops::ControlFlow};

use super::Expression;

/// Generic trait that allows to iterate over sub-structures.
/// It is only meant to iterate non-recursively over the direct children.
/// Self and O do not have to be the same type and we can also have
/// Children<O1> and Children<O2> implemented for the same type,
/// if the goal is to iterate over sub-structures of different kinds.
pub trait Children<O> {
    /// Returns an iterator over all direct children of kind O in this object.
    fn children(&self) -> Box<dyn Iterator<Item = &O> + '_>;
    /// Returns an iterator over all direct children of kind Q in this object.
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut O> + '_>;
}

pub trait AllChildren<O> {
    /// Returns an iterator over all direct and indirect children of kind O in this object.
    /// Pre-order visitor.
    fn all_children(&self) -> Box<dyn Iterator<Item = &O> + '_>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VisitOrder {
    Pre,
    Post,
}

/// A trait to be implemented by an AST node.
/// The idea is that it calls a callback function on each of the sub-nodes
/// that are expressions.
/// The difference to the Children<Expr> trait is that ExpressionVisitable
/// visits recursively.
/// If a node implements Children<Expr>, it also implements ExpressionVisitable<Expr>.
pub trait ExpressionVisitable<Expr> {
    /// Traverses the AST and calls `f` on each Expression in pre-order,
    /// potentially break early and return a value.
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expr) -> ControlFlow<B>,
    {
        self.visit_expressions_mut(f, VisitOrder::Pre)
    }

    /// Traverses the AST and calls `f` on each Expression in pre-order.
    fn pre_visit_expressions_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Expr),
    {
        self.pre_visit_expressions_return_mut(&mut move |e| {
            f(e);
            ControlFlow::Continue::<()>(())
        });
    }

    /// Traverses the AST and calls `f` on each Expression in pre-order,
    /// potentially break early and return a value.
    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expr) -> ControlFlow<B>,
    {
        self.visit_expressions(f, VisitOrder::Pre)
    }

    /// Traverses the AST and calls `f` on each Expression in pre-order.
    fn pre_visit_expressions<F>(&self, f: &mut F)
    where
        F: FnMut(&Expr),
    {
        self.pre_visit_expressions_return(&mut move |e| {
            f(e);
            ControlFlow::Continue::<()>(())
        });
    }

    /// Traverses the AST and calls `f` on each Expression in post-order,
    /// potentially break early and return a value.
    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut Expr) -> ControlFlow<B>,
    {
        self.visit_expressions_mut(f, VisitOrder::Post)
    }

    /// Traverses the AST and calls `f` on each Expression in post-order.
    fn post_visit_expressions_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Expr),
    {
        self.post_visit_expressions_return_mut(&mut move |e| {
            f(e);
            ControlFlow::Continue::<()>(())
        });
    }

    /// Traverses the AST and calls `f` on each Expression in post-order,
    /// potentially break early and return a value.
    fn post_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&Expr) -> ControlFlow<B>,
    {
        self.visit_expressions(f, VisitOrder::Post)
    }

    /// Traverses the AST and calls `f` on each Expression in post-order.
    fn post_visit_expressions<F>(&self, f: &mut F)
    where
        F: FnMut(&Expr),
    {
        self.post_visit_expressions_return(&mut move |e| {
            f(e);
            ControlFlow::Continue::<()>(())
        });
    }

    fn visit_expressions<F, B>(&self, f: &mut F, order: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expr) -> ControlFlow<B>;

    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, order: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expr) -> ControlFlow<B>;

    fn expr_any(&self, mut f: impl FnMut(&Expr) -> bool) -> bool {
        self.pre_visit_expressions_return(&mut |e| {
            if f(e) {
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            }
        })
        .is_break()
    }
}

impl<Ref> ExpressionVisitable<Expression<Ref>> for Expression<Ref> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expression<Ref>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        self.children_mut()
            .try_for_each(|child| child.visit_expressions_mut(f, o))?;
        if o == VisitOrder::Post {
            f(self)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expression<Ref>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        self.children()
            .try_for_each(|child| child.visit_expressions(f, o))?;
        if o == VisitOrder::Post {
            f(self)?;
        }
        ControlFlow::Continue(())
    }
}

impl<Expr: ExpressionVisitable<Expr>, C: Children<Expr>> ExpressionVisitable<Expr> for C {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut Expr) -> ControlFlow<B>,
    {
        self.children_mut()
            .try_for_each(|child| child.visit_expressions_mut(f, o))
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&Expr) -> ControlFlow<B>,
    {
        self.children()
            .try_for_each(|child| child.visit_expressions(f, o))
    }
}

impl<Ref> AllChildren<Expression<Ref>> for Expression<Ref> {
    fn all_children(&self) -> Box<dyn Iterator<Item = &Expression<Ref>> + '_> {
        Box::new(iter::once(self).chain(self.children().flat_map(|e| e.all_children())))
    }
}

impl<Expr: AllChildren<Expr>, C: Children<Expr>> AllChildren<Expr> for C {
    fn all_children(&self) -> Box<dyn Iterator<Item = &Expr> + '_> {
        Box::new(self.children().flat_map(|e| e.all_children()))
    }
}
