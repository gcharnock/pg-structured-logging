
-- Constants

CREATE FUNCTION public.critical() RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $$SELECT 45$$;

CREATE FUNCTION public.error() RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $$SELECT 35$$;

CREATE FUNCTION public.warn() RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $$SELECT 25$$;

CREATE FUNCTION public.info() RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $$SELECT 15$$;


CREATE FUNCTION public.trace() RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $$SELECT 5$$;

-- partition names

CREATE FUNCTION public.get_event_table_partition_name(t timestamp with time zone) RETURNS TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  lower_bound timestamp with time zone := date_trunc('day', t);
BEGIN
  RETURN format('event_%s_%s_%s', date_part('year', lower_bound), date_part('month', lower_bound), date_part('day', lower_bound));
END
$$;

CREATE FUNCTION public.get_message_table_partition_name(t timestamp with time zone) RETURNS TEXT
  LANGUAGE plpgsql
  AS $$
  DECLARE
    lower_bound timestamp with time zone;
  BEGIN
    lower_bound := date_trunc('day', t);
    RETURN format('message_%s_%s_%s', date_part('year', lower_bound), date_part('month', lower_bound), date_part('day', lower_bound));
  END;
$$;

-- partitioners

CREATE FUNCTION public.make_partition_for_event(t timestamp with time zone) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
  lower_bound timestamp := date_trunc('day', t);
  upper_bound timestamp := date_trunc('day', t + interval '1 day');
  table_name text := get_event_table_partition_name(t);
BEGIN
  execute format('create table %s PARTITION OF public.event FOR VALUES FROM (''%s'') TO (''%s'')', table_name, lower_bound, upper_bound);
  RETURN NULL;
END;
$$;


CREATE FUNCTION public.make_partition_for_message(t timestamp with time zone) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
  lower_bound timestamp := date_trunc('day', t);
  upper_bound timestamp := date_trunc('day', t + interval '1 day');
  table_name text := get_message_table_partition_name(t);
BEGIN
  execute format('create table %s PARTITION OF public.message FOR VALUES FROM (''%s'') TO (''%s'') PARTITION BY RANGE (level)', table_name, lower_bound, upper_bound);
  execute format('create table %s PARTITION OF %s FOR VALUES FROM (-32768) TO (10)', table_name || '_trace', table_name);
  execute format('create table %s PARTITION OF %s FOR VALUES FROM (11) TO (20)', table_name || '_info', table_name);
  execute format('create table %s PARTITION OF %s FOR VALUES FROM (20) TO (32767)', table_name || '_error', table_name);
  RETURN NULL;
END;
$$;

-- triggers

CREATE FUNCTION route_event()
  RETURNS TRIGGER AS $$
DECLARE
  tablename TEXT := get_event_table_partition_name(NEW.timestamp);
BEGIN
  IF NOT EXISTS (SELECT relname FROM pg_class WHERE relname=tablename)
  THEN
    PERFORM make_partition_for_event(NEW.timestamp);
  END IF;
  INSERT INTO public.event (SELECT (NEW).*);
  RETURN null;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE TRIGGER insert_to_event_trigger
    BEFORE INSERT
    ON inserters.event
    FOR EACH ROW
    EXECUTE PROCEDURE route_event();


CREATE FUNCTION route_message()
  RETURNS TRIGGER AS $$
DECLARE
  tablename TEXT := get_message_table_partition_name(NEW.timestamp);
BEGIN
  IF NOT EXISTS (SELECT relname FROM pg_class WHERE relname=tablename)
  THEN
    PERFORM make_partition_for_message(NEW.timestamp);
  END IF;
  INSERT INTO message (SELECT (NEW).*);
  RETURN null;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE TRIGGER insert_to_message_trigger
    BEFORE INSERT
    ON inserters.message
    FOR EACH ROW
    EXECUTE PROCEDURE route_message();


