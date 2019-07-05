CREATE SEQUENCE event_event_id_seq;
CREATE SEQUENCE message_id_seq;

CREATE TABLE public.event
(
    event_id uuid NOT NULL,
    timestamp_start timestamp with time zone NOT NULL,
    timestamp_end timestamp with time zone,
    parent uuid,
    event_type text COLLATE pg_catalog."default",
    data jsonb,
    error jsonb,
    result jsonb,
    CONSTRAINT event_pkey PRIMARY KEY (event_id)
)
WITH (
    OIDS = FALSE
);


CREATE UNLOGGED TABLE public.message
(
    message_id bigint NOT NULL DEFAULT nextval('message_id_seq'::regclass),
    message text COLLATE pg_catalog."default" NOT NULL,
    level text COLLATE pg_catalog."default" NOT NULL,
    event_id UUID,
    timestamp timestamp with time zone NOT NULL,
    filename text COLLATE pg_catalog."default",
    line integer,
    col integer,
    data jsonb,
    CONSTRAINT message_pk PRIMARY KEY (message_id)
)
WITH (
    OIDS = FALSE
)

CREATE OR REPLACE FUNCTION parition_message(t timestamp with time zone) RETURNS boolean AS $$
DECLARE
  lower_bound timestamp := date_trunc('day', t);
  upper_bound timestamp := date_trunc('day', t + interval '1 day');
  table_name text := format('message_%s_%s_%s', date_part('year', lower_bound), date_part('month', lower_bound), date_part('day', lower_bound));
BEGIN
  execute format('create table %s PARTITION OF message FOR VALUES FROM (''%s'') TO (''%s'') PARTITION BY RANGE (level)', table_name, lower_bound, upper_bound);
  execute format('create table %s PARTITION OF %s FOR VALUES FROM (-32768) TO (10)', table_name || '_trace', table_name);
  execute format('create table %s PARTITION OF %s FOR VALUES FROM (11) TO (20)', table_name || '_info', table_name);
  execute format('create table %s PARTITION OF %s FOR VALUES FROM (20) TO (32767)', table_name || '_error', table_name);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

