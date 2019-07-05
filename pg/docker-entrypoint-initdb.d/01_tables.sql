
CREATE SEQUENCE public.message_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

CREATE TYPE event_type AS (
    event_id uuid,
    timestamp_start timestamp with time zone,
    timestamp_end timestamp with time zone,
    refs uuid[],
    parent uuid,
    event_type text,
    data jsonb,
    error jsonb,
    result jsonb
);

CREATE TABLE public.event OF event_type (
    event_id NOT NULL,
    timestamp_start NOT NULL
)
PARTITION BY RANGE (timestamp_start);

CREATE TABLE inserters.event OF event_type (
    event_id NOT NULL,
    timestamp_start NOT NULL
);

CREATE TYPE message_type AS (
    message_id bigint,
    message text,
    level smallint,
    event_id uuid,
    "timestamp" timestamp with time zone,
    topics text[],
    filename text,
    line integer,
    col integer,
    data jsonb
);

CREATE TABLE inserters.message OF message_type (
    message_id WITH OPTIONS DEFAULT nextval('public.message_id_seq'::regclass) NOT NULL,
    message NOT NULL,
    level NOT NULL,
    "timestamp" WITH OPTIONS DEFAULT now() NOT NULL
);

CREATE TABLE public.message OF message_type (
    message_id WITH OPTIONS DEFAULT nextval('public.message_id_seq'::regclass) NOT NULL,
    message NOT NULL,
    level NOT NULL,
    "timestamp" NULL
)
PARTITION BY RANGE ("timestamp");


