CREATE TABLE IF NOT EXISTS responses (
  art_id TEXT NOT NULL,
  voter_hash TEXT NOT NULL,
  location TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (art_id, voter_hash, location)
);

CREATE INDEX IF NOT EXISTS responses_art_id_idx ON responses (art_id);
