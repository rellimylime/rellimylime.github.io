const ART_IDS = new Set([
  'remains', 'still-see-you', 'agony', 'pained-smile', 'candy-clown',
  'sheltered', 'happy-place', 'octo-tangle', 'artists-collab', 'akin',
  'ancient-bond', 'maternal-patience', 'natural-interior', 'ocean-painting',
  'smoking-woman', 'twisted-pose', 'what-god'
]);

const LOCATIONS = new Set([
  'jaw', 'throat', 'chest', 'gut', 'back', 'hands', 'skin', 'head',
  'somewhere-else', 'nowhere-yet'
]);

function headers(origin) {
  return {
    'Access-Control-Allow-Origin': origin,
    'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
    'Access-Control-Allow-Headers': 'Content-Type',
    'Content-Type': 'application/json; charset=utf-8',
    'Vary': 'Origin'
  };
}

function json(value, status, origin) {
  return new Response(JSON.stringify(value), { status, headers: headers(origin) });
}

async function hashVoter(voterId, salt) {
  const bytes = new TextEncoder().encode(`${salt}:${voterId}`);
  const digest = await crypto.subtle.digest('SHA-256', bytes);
  return [...new Uint8Array(digest)].map((byte) => byte.toString(16).padStart(2, '0')).join('');
}

async function counts(db, artId) {
  const result = await db.prepare(
    'SELECT location, COUNT(*) AS count FROM responses WHERE art_id = ? GROUP BY location'
  ).bind(artId).all();
  return Object.fromEntries([...LOCATIONS].map((location) => [
    location,
    Number(result.results.find((row) => row.location === location)?.count || 0)
  ]));
}

export default {
  async fetch(request, env) {
    const requestOrigin = request.headers.get('Origin');
    const allowedOrigin = env.ALLOWED_ORIGIN;
    const origin = requestOrigin === allowedOrigin ? allowedOrigin : allowedOrigin;

    if (request.method === 'OPTIONS') return new Response(null, { status: 204, headers: headers(origin) });
    if (requestOrigin && requestOrigin !== allowedOrigin) return json({ error: 'Origin not allowed' }, 403, origin);

    const url = new URL(request.url);
    const artId = url.searchParams.get('art_id');

    if (request.method === 'GET') {
      if (!ART_IDS.has(artId)) return json({ error: 'Unknown artwork' }, 400, origin);
      return json({ art_id: artId, counts: await counts(env.DB, artId) }, 200, origin);
    }

    if (request.method !== 'POST') return json({ error: 'Method not allowed' }, 405, origin);

    let body;
    try { body = await request.json(); } catch { return json({ error: 'Invalid JSON' }, 400, origin); }
    if (!ART_IDS.has(body.art_id)) return json({ error: 'Unknown artwork' }, 400, origin);
    if (typeof body.voter_id !== 'string' || body.voter_id.length < 8 || body.voter_id.length > 100) {
      return json({ error: 'Invalid voter token' }, 400, origin);
    }
    const selected = [...new Set(Array.isArray(body.locations) ? body.locations : [])];
    if (!selected.length || selected.length > 10 || selected.some((location) => !LOCATIONS.has(location))) {
      return json({ error: 'Invalid body locations' }, 400, origin);
    }

    const voterHash = await hashVoter(body.voter_id, env.VOTER_SALT);
    const statements = [
      env.DB.prepare('DELETE FROM responses WHERE art_id = ? AND voter_hash = ?').bind(body.art_id, voterHash),
      ...selected.map((location) => env.DB.prepare(
        'INSERT INTO responses (art_id, voter_hash, location) VALUES (?, ?, ?)'
      ).bind(body.art_id, voterHash, location))
    ];
    await env.DB.batch(statements);
    return json({ art_id: body.art_id, counts: await counts(env.DB, body.art_id) }, 200, origin);
  }
};
