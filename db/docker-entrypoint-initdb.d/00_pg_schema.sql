CREATE SEQUENCE event_event_id_seq;
CREATE SEQUENCE message_id_seq;

CREATE UNLOGGED TABLE public.event
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
