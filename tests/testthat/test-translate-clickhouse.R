library("testthat")

# For debugging: force reload of patterns:
# rJava::J('org.ohdsi.sql.SqlTranslate')$setReplacementPatterns('inst/csv/replacementPatterns.csv')

expect_equal_ignore_spaces <- function(string1, string2) {
  string1 <- gsub("([;()'+-/|*\n])", " \\1 ", string1)
  string2 <- gsub("([;()'+-/|*\n])", " \\1 ", string2)
  string1 <- gsub(" +", " ", string1)
  string2 <- gsub(" +", " ", string2)
  expect_equivalent(string1, string2)
}

test_that("translate sql server -> ClickHouse select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select column from (select column, row_number() over (order by md5(toString(person_id))) tmp where rn <= 1"
  )
})

test_that("translate sql server -> ClickHouse SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select row_number() over reinterpretAsInt64(unhex(val)) rn where rn <= 1"
  )
})

test_that("translate sql server -> clickhouse lowercase all but strings and variables", {
  sql <- translate("SELECT X.Y, 'Mixed Case String' FROM \"MixedCaseTableName.T\" where x.z=@camelCaseVar GROUP BY X.Y",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select x.y, 'Mixed Case String' from `MixedCaseTableName.T` where x.z=@camelCaseVar group by x.y"
  )
})

test_that("translate sql server -> clickhouse common table expression column list", {
  sql <- translate("with cte(x, y, z) as (select c1, c2 as y, c3 as r from t) select x, y, z from cte;",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "with cte as (select c1 as x, c2 as y, c3 as z from t) select x, y, z from cte;"
  )
})

test_that("translate sql server -> clickhouse distinct keyword", {
  sql <- translate("with cte2 (column1, column2) as (select distinct c1.column1, c1.column2 from cte c1) select column1, column2 into cte2 from cte2",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "with cte2 as (select distinct c1.column1 as column1, c1.column2 as column2 from cte c1) create table cte2 as select column1, column2 from cte2"
  )
})

test_that("translate sql server -> clickhouse group by function", {
  sql <- translate("select f(a), count(*) from t group by f(a);", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select f(a), count(*) from t group by f(a);")
})

test_that("translate sql server -> clickhouse DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select date_diff('day', cast(drug_era_start_date as DateTime64), cast(drug_era_end_date as DateTime64)) from drug_era;"
  )
})

test_that("translate sql server -> clickhouse DATEDIFF year", {
  sql <- translate("SELECT DATEDIFF(YEAR,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select date_diff('year', cast(drug_era_start_date as DateTime64), cast(drug_era_end_date as DateTime64)) from drug_era;"
  )
})

test_that("translate sql server -> clickhouse DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select date_add(cast(drug_era_end_date as DateTime64), interval 30 day) from drug_era;"
  )
})

test_that("translate sql server -> clickhouse DATEADD non-integer", {
  sql <- translate("SELECT DATEADD(dd,30.0,drug_era_end_date) FROM drug_era;",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select date_add(cast(drug_era_end_date as DateTime64), interval 30 day) from drug_era;"
  )
})

test_that("translate sql server -> clickhouse GETDATE", {
  sql <- translate("GETDATE()", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toDate32(now())")
})

test_that("translate sql server -> clickhouse STDEV", {
  sql <- translate("stdev(x)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "stddevPop(x)")
})

test_that("translate sql server -> clickhouse LEN", {
  sql <- translate("len('abc')", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "lengthUTF8('abc')")
})

test_that("translate sql server -> clickhouse COUNT_BIG", {
  sql <- translate("COUNT_BIG(x)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "count(x)")
})

test_that("translate sql server -> clickhouse CAST varchar", {
  sql <- translate("select cast(x as varchar)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select toString(x)")
})

test_that("translate sql server -> clickhouse CAST :float", {
  sql <- translate("select cast(x as:float)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select toFloat64(x)")
})

test_that("translate sql server -> clickhouse DROP TABLE IF EXISTS", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(sql, "drop table if exists cohort;")
})

test_that("translate sql server -> clickhouse CAST string", {
  sql <- translate("CAST(x AS VARCHAR(255))", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toString(x)")
})

test_that("translate sql server -> clickhouse LEFT, RIGHT", {
  sql <- translate("select LEFT(a, 20), RIGHT(b, 30) FROM t;", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select SUBSTR(a, 0, 20), SUBSTR(b, -30) from t;")
})

test_that("translate sql server -> clickhouse cast float", {
  sql <- translate("cast(a as float)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toFloat64(a)")
})

test_that("translate sql server -> clickhouse cast bigint", {
  sql <- translate("cast(a as bigint)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toInt64(a)")
})

test_that("translate sql server -> clickhouse cast int", {
  sql <- translate("cast(a as int)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toInt64(a)")
})

test_that("translate sql server -> clickhouse cast date", {
  sql <- translate("date(d)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toDate32(d)")
})

test_that("translate sql server -> clickhouse cast concat string as date", {
  sql <- translate("cast(concat(a,b) as date)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toDate32(concat(a,b))")
})

test_that("translate sql server -> clickhouse cast string as date", {
  sql <- translate("cast(a as date)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toDate32(a)")
})

test_that("translate sql server -> clickhouse extract year", {
  sql <- translate("year(d)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toYear(d)")
})

test_that("translate sql server -> clickhouse extract month", {
  sql <- translate("month(d)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toMonth(d)")
})

test_that("translate sql server -> clickhouse extract day", {
  sql <- translate("day(d)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "toDay(d)")
})

test_that("translate sql server -> clickhouse union distinct", {
  sql <- translate("select 1 as x union select 2;", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select 1 as x union distinct select 2;")
})

test_that("translate sql server -> clickhouse intersect distinct", {
  sql <- translate("SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s;",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select distinct a from t intersect distinct select distinct a from s;"
  )
})

test_that("translate sql server -> clickhouse isnull", {
  sql <- translate("SELECT isnull(x,y) from t;", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select ifNull(x,y) from t;")
})

test_that("translate sql server -> clickhouse unquote aliases", {
  sql <- translate("SELECT a as \"b\" from t;", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select a as b from t;")
})

test_that("translate sql server -> clickhouse cast decimal", {
  sql <- translate("select cast(x as decimal(18,4)) from t", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select toFloat64(x) from t")
})

test_that("translate sql server -> clickhouse ISNUMERIC", {
  sql <- translate("select ISNUMERIC(a) from b", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(
    sql,
    "select isNotNull(toFloat64OrNull(toString(a))) from b"
  )
})

test_that("translate sql server -> clickhouse index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(sql, "-- bigquery does not support indexes")
})

test_that("translate sql server -> clickhouse TRUNCATE TABLE", {
  sql <- translate("TRUNCATE TABLE cohort;", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "truncate table cohort;")
})

test_that("translate sql server -> clickhouse DATEFROMPARTS", {
  sql <- translate("select DATEFROMPARTS(2019,1,30)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select makeDate32(2019, 1, 30)")
})

test_that("translate sql server -> clickhouse EOMONTH()", {
  sql <- translate("select eomonth(payer_plan_period_start_date)", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(
    sql,
    "select toLastDayOfMonth(payer_plan_period_start_date)"
  )
})

test_that("translate sql server -> clickhouse UPDATE STATISTICS", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "-- not a thing in clickhouse")
})

test_that("translate sql server -> clickhouse modulus", {
  sql <- translate("SELECT row_number() over (order by cast(person_id % 123 as int))",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select row_number() over (order by toInt64(person_id % 123))"
  )
})

test_that("translate sql server -> clickhouse String concatenation", {
  sql <- translate("SELECT last_name + ', ' + first_name FROM my_table;",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select last_name || ', ' || first_name from my_table;"
  )
})

test_that("translate sql server -> clickhouse String concatenation with CAST", {
  sql <- translate("SELECT first_name + CAST(middle_initial AS VARCHAR) + last_name FROM my_table;",
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select first_name || toString(middle_initial) || last_name from my_table;"
  )
})

test_that("translate sql server -> clickhouse DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS test;", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "drop table if exists test;")
})

test_that("translate sql server -> clickhouse nullable field", {
  sql <- translate("CREATE TABLE test (x NUMERIC NULL, y NUMERIC NOT NULL);", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "create table test (x Float64, y Float64 not null);")
})

test_that("translate sql server -> clickhouse NEWID()", {
  sql <- translate("SELECT *, NEWID() FROM my_table;", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select *, generateUUIDv4() from my_table;")
})

test_that("translate sql server -> clickhouse IIF", {
  sql <- translate("SELECT IIF(a>b, 1, b) AS max_val FROM table;", targetDialect = "clickhouse")
  expect_equal_ignore_spaces(sql, "select multiIf(a>b, 1, b) as max_val from table;")
})

test_that("translate sql server -> clickhouse CREATE TABLE IF NOT EXISTS", {
  sql <- translate("IF OBJECT_ID('my_table', 'U') IS NULL CREATE TABLE my_table (id INT);", 
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(sql, "create table if not exists my_table (id Int64);")
})

test_that("translate sql server -> clickhouse VALUES clause", {
  sql <- translate("SELECT * FROM (VALUES (1,2), (3,4)) AS t(a,b);", 
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql,
    "select * from (select NULL AS a, NULL AS b union all select 1, 2 union all select 3, 4 limit 999999 offset 1) as values_table;"
  )
})

test_that("translate sql server -> clickhouse temp table creation", {
  sql <- translate("CREATE TABLE #temp_table (id INT);", 
    targetDialect = "clickhouse",
    tempEmulationSchema = "temp"
  )
  expect_equal_ignore_spaces(
    sql, 
    sprintf("drop table if exists temp.%stemp_table;\ncreate table temp.%stemp_table (id Int64);", getTempTablePrefix(), getTempTablePrefix())
  )
})

test_that("translate sql server -> clickhouse DATEPART", {
  sql <- translate("SELECT DATEPART(YEAR, event_date) FROM events;", 
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(sql, "select toYear(event_date) from events;")
})

test_that("translate sql server -> clickhouse SELECT INTO", {
  sql <- translate("SELECT person_id, drug_id INTO #drugs FROM drug_exposure;", 
    targetDialect = "clickhouse",
    tempEmulationSchema = "temp"
  )
  expect_equal_ignore_spaces(
    sql, 
    sprintf("drop table if exists temp.%sdrugs;\ncreate table temp.%sdrugs as\nselect\nperson_id, drug_id\nfrom\ndrug_exposure;", getTempTablePrefix(), getTempTablePrefix())
  )
})

test_that("translate sql server -> clickhouse window functions", {
  sql <- translate("SELECT ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY drug_date) FROM drug_exposure;", 
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql, 
    "select row_number() over (partition by person_id order by drug_date) from drug_exposure;"
  )
})

test_that("translate sql server -> clickhouse window function with frame", {
  sql <- translate("SELECT SUM(cost) OVER (PARTITION BY person_id ORDER BY drug_date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM drug_exposure;", 
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql, 
    "select sum(cost) over (partition by person_id order by drug_date rows between unbounded preceding and current row) from drug_exposure;"
  )
})

test_that("translate sql server -> clickhouse CONCAT function", {
  sql <- translate("SELECT CONCAT(first_name, ' ', last_name) FROM person;", 
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql, 
    "select first_name || ' ' || last_name from person;"
  )
})

test_that("translate sql server -> clickhouse CHARINDEX function", {
  sql <- translate("SELECT CHARINDEX('abc', field) FROM table;", 
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql, 
    "select positionUTF8(field, 'abc') from table;"
  )
})

test_that("translate sql server -> clickhouse CREATE TABLE with engine", {
  sql <- translate("CREATE TABLE my_table (id INT, name VARCHAR(100));", 
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql, 
    "create table my_table (id Int64, name String);"
  )
})

test_that("translate sql server -> clickhouse WITH clause and INSERT", {
  sql <- translate("WITH src AS (SELECT id, name FROM source_table) INSERT INTO dest_table SELECT * FROM src;", 
    targetDialect = "clickhouse"
  )
  expect_equal_ignore_spaces(
    sql, 
    "insert into dest_table with src as (select id, name from source_table) select * from src;"
  )
}) 