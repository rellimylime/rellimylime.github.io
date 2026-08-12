# Collective body-response tally

This Cloudflare Worker + D1 service stores aggregate responses for the art gallery. A browser gets a random local token; the service hashes it with a secret salt and stores only that hash, the artwork ID, and selected body locations. Submitting again replaces that browser's earlier response for the same piece.

## Deploy

1. Install Wrangler and sign in: `npm install -g wrangler`, then `wrangler login`.
2. Create the database: `wrangler d1 create emily-body-responses`.
3. Copy `wrangler.toml.example` to `wrangler.toml` and insert the returned database ID.
4. Apply the schema: `wrangler d1 execute emily-body-responses --remote --file=schema.sql`.
5. Create a random secret: `wrangler secret put VOTER_SALT`.
6. Deploy: `wrangler deploy`.
7. Set `window.EMILY_BODY_RESPONSE_API` in the website to the deployed Worker URL.

The public page intentionally shows the collective results only after a visitor submits a response. The endpoint still supports `GET` for debugging and future displays.
