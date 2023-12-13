# MiniSQL
## Descripción general de la aplicación
**MiniSQL** es un [Lenguaje Específico de Dominio](https://es.wikipedia.org/wiki/Lenguaje_espec%C3%ADfico_de_dominio) (DSL) que replica de manera simplificada el lenguaje SQL.

Con esto nos referimos a que el DSL tiene implementado únicamente estos cinco comandos del lenguaje SQL original:
- **CREATE DATABASE**
- **CREATE TABLE**
- **INSERT**
- **SELECT**
- **DELETE**

Toda la documentación correspondiente, incluyendo casos de uso, funciones y sintaxis soportada de los comandos, se encuentran [aquí](https://drive.google.com/file/d/16qwS-vU-q-zNDNoT_UKqNU9QvLrEeJtp/view?usp=sharing).

## Uso
### Requerimientos
[Haskell GHC](https://www.haskell.org/ghc/) o Docker.

### Ejecución
### Haskell GHC
1. Compilar el proyecto: `ghc -odir bin -hidir bin Main.hs`
>Con `-odir` y `-hidir` lo que hacemos es dirigir los archivos binarios resultantes de la compilación a un directorio en específico, para que no queden mezclados con el resto del código. En este caso, se almacenaran en el directorio */bin*.
2. Crear un archivo de consultas.
>**IMPORTANTE**: el nombre del archivo de consultas es a elección, pero la extensión del mismo **debe** ser .minsql. De lo contrario, no será tomado como válido.
3. Ejecutar el proyecto: `./Main <nombre_archivo_consulta>`
>Al ejecutar, el programa va imprimiendo por consola los resultados de los comandos existentes en el archivo de consulta.

### Docker
1. Hacer pull de la imagen: `docker pull thiagoqua/minisql:1.0`
2. Crear un archivo de consultas.
>**IMPORTANTE**: el nombre del archivo de consultas es a elección, pero la extensión del mismo **debe** ser .minsql. De lo contrario, no será tomado como válido.
3. Correr el contenedor: `docker run --rm -v $(pwd):/app/ext -v $(pwd)/database:/app/database -e QUERY_FNAME=<nombre_archivo> thiagoqua/minisql:1.0`
>Reemplazar la variable de entorno *nombre_archivo* por el nombre del archivo de consultas creado completo, incluyendo su extensión .minsql.

<br/>

Una vez terminada la ejecución en cualquiera de ambos escenarios, se puede **modificar** el archivo de consultas las veces que se deseen y volver a ejecutar el programa para que aplique los nuevos cambios a la base de datos, tal como detalla el punto 3.

## Consulta de ejemplo

```sql
create database productos;

use productos;

create table telefono (
    id-integer,marca-string-10,modelo-string-20,color-string-10
);

create table auto (
    id-integer,marca-string-20,modelo-string-10,anio-integer
);
insert into auto values 
    (1,"toyota","corolla",2020),
    (2,"ford","focus",2019),
    (3,"nissan","sentra",2021),
    (4,"chevrolet","cruze",2018),
    (5,"honda","civic",2019);

insert into telefono values
    (1,"samsung","galaxy s21","negro"),
    (2,"iphone","12 pro","negro"),
    (3,"google","pixel 6","blanco"),
    (4,"oneplus","9 pro","azul"),
    (5,"xiaomi","mi 11","negro");

select * from auto;

select id, marca as m, modelo from telefono where color = "negro" order by m desc;

delete from auto;

delete from telefono where id = 3 or marca = "iphone";
```
