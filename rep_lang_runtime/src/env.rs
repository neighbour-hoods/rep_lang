use core::iter::FromIterator;
use std::{collections::HashMap, ops::Deref};

use super::syntax::*;
use super::types::*;

#[derive(Clone, Debug)]
pub struct Env(HashMap<Name, Scheme>);

impl Env {
    pub fn new() -> Env {
        Env(HashMap::new())
    }

    pub fn extend(&mut self, nm: Name, sc: Scheme) -> Option<Scheme> {
        match self {
            Env(hm) => hm.insert(nm, sc),
        }
    }

    pub fn remove(&mut self, nm: Name) -> Option<Scheme> {
        match self {
            Env(hm) => hm.remove(&nm),
        }
    }

    pub fn extends<T>(&mut self, xs: T)
    where
        T: IntoIterator<Item = (Name, Scheme)>,
    {
        let Env(ref mut hm) = self;
        hm.extend(xs)
    }

    pub fn get(&self, nm: &Name) -> Option<&Scheme> {
        match self {
            Env(hm) => hm.get(nm),
        }
    }

    pub fn merge(&mut self, other: &Env) {
        let Env(ref mut hm) = self;
        let Env(other_hm) = other;
        // TODO is this avoidable waste?
        hm.extend(other_hm.clone())
    }

    pub fn merge_envs(&mut self, others: Vec<Env>) {
        for env in others {
            self.merge(&env);
        }
    }

    pub fn singleton(nm: Name, sc: Scheme) -> Env {
        let mut env = Env::new();
        env.extend(nm, sc);
        env
    }

    pub fn keys<T>(&self) -> Vec<Name> {
        let Env(hm) = self;
        // TODO is this avoidable waste?
        hm.keys().map(|x| x.clone()).collect()
    }

    pub fn replace(&mut self, nm: &Name, sc: Scheme) {
        self.remove(nm.clone());
        self.extend(nm.clone(), sc);
    }
}

impl FromIterator<(Name, Scheme)> for Env {
    fn from_iter<T: IntoIterator<Item = (Name, Scheme)>>(iter: T) -> Env {
        let hm: HashMap<Name, Scheme> = HashMap::from_iter(iter);
        Env(hm)
    }
}

impl Deref for Env {
    type Target = HashMap<Name, Scheme>;

    fn deref(&self) -> &Self::Target {
        let Env(hm) = self;
        &hm
    }
}
