import * as L from "https://deno.land/x/lucid@0.10.5/mod.ts";
import { secretSeed } from "./seed.ts";

const lucid: L.Lucid = await L.Lucid.new(
    new L.Kupmios(
      "http://127.0.0.3:1442",
      "ws://127.0.0.3:1337",
    ),
    "Preview",
);
