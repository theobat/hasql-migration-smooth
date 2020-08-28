massalia-migration
===================

This library facilitates the process of migrating, documenting and creating a Postgresql schema.
The main idea and motivation is that we usually organise our projects in a list of modules where
each folder represent a module (which may, or may not, contain one or several SQL tables).

A large majority of database migration tools, expect us to put SQL files into a dedicated migration folder 
which is basically the kitchen's sink of the database, we put everything into it with no consideration
for what's actually done in the scripts. The only guarantee beeing that, scripts are executed only once, and
in their creation order (or some other order).

The idea of a component however is that everything that's related to it can be found in it.
This library gives the ability to define a bunch of files in their component and provides 3
different kind of migrations:
  
  - The data definition language init (ddli* files by default)
  - The data definition language revision (ddlr* files by default)
  - The data manipulation language seeds (dml by default)

The workflow is the following:

  - 
