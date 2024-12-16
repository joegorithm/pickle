// Determine year for copyright notice
document.addEventListener("DOMContentLoaded", function() {
    var copyright = document.getElementById("copyright");

    function setCopyrightDate() {
        const currentDate = new Date();
        const currentYear = currentDate.getFullYear();
        copyright.textContent = "© " + currentYear + " JH.codes";
    }

    setCopyrightDate();
});

var programCode = function(processingInstance) {
    with (processingInstance) {
      size(599, 600);
      frameRate(60);



// CREDITS ======================================================
/** -------------------------------------------------------------
Pickle is based off of Wordle by the New York Times Company.
------------------------------------------------------------- **/



// SETUP ========================================================
textAlign(CENTER, CENTER); // align text according to its center
rectMode(CENTER); // align rectangles according to their centers
noStroke(); // no outlines
strokeCap(ROUND); // round ends of lines and arcs



// VARIABLES ====================================================

// mouse and keyboard -------------
var mouseOverCanvas = true; // true if mouse is over canvas
var enter = false; // true if enter is typed or pressed
var letterLimit = 5; // maximum number of letters you can type
var backspaceLimit = 0; // limit for backspacing
var typeBackspaceRestrictor = 0; // limit for typing backspace

// introduction -------------------
var introOpacity = 255; // opacity of introduction background
var introX = -300; // x-position of Pickle logo intro
var introXRate; // rate of change of x-position of intro
var introTime = 0; // time during the introduction
var intro = true; // if true, play the introduction

// scene management ---------------
var currentTry = 0; // current word guess
var play = false; // if true, enable game controls (typing)
var darkenOpacity = 0; // opacity of darkened background
var darkenBackground = false; // if true, darken background
var howToPlayStatus = false; // if true, display "How to Play"
var statisticsStatus = false; // if true, display "Statistics"
var settingsStatus = false; // if true, display "Settings"
var revealThumbnail = false; // if true, reveal thumbnail

// cells, words, & letters --------
var keyAmount = 0; // amount of keys displayed during start-up
var determineCellColors = false; // determine each cell color
var moveCells = false; // if true, move cells out for new ones
var moveCellsTime = 0; // time during the moving of the cells
var cellsX = 600; // x-position of the cells
var specificRow; // specific row during current try
var specificRowLetter; // specific row letter during current try
var specificWord; // specific word during current try
var cellFlipTime = 0; // time during flipping cell tiles

// settings -----------------------
var darkMode = false; // true if dark mode is on
var WordleLink = "https://www.nytimes.com/games/wordle/index.html"; // link to the real Wordle

// win or lose --------------------
var gameOverTime = 0; // time during the game over period
var gameOver = false; // true if the game is over
var win = false; // true if the game was won
var lose = false; // true if the game was lost
var winStatus = false; // if true, display the "win" pop-up
var loseStatus = false; // if true, display the "lose" pop-up

// sound --------------------------
var soundStatus = true; // if true, allow sounds to be played
var playIncorrectSound = false; // if true, play incorrect sound
var incorrectSoundTime = 0; // time during the incorrect sound

// statistics ---------------------
var statsGamesPlayed = 0; // number of games played
var statsPercentWon; // percentage of games won
var statsGamesWon = 0; // number of games won
var statsMaxStreak; // max streak
var statsCurrentStreak; // current streak
var statsTry1 = 0; // number of games won on try one
var statsTry2 = 0; // number of games won on try two
var statsTry3 = 0; // number of games won on try three
var statsTry4 = 0; // number of games won on try four
var statsTry5 = 0; // number of games won on try five
var statsTry6 = 0; // number of games won on try six
var statsBarWidth1; // width of histogram bar #1
var statsBarWidth2; // width of histogram bar #2
var statsBarWidth3; // width of histogram bar #3
var statsBarWidth4; // width of histogram bar #4
var statsBarWidth5; // width of histogram bar #5
var statsBarWidth6; // width of histogram bar #6


// create a mapping of keyCodes to CAPITAL letter
var keyCodeToLetter = {}; // keyCode to letter mapping
var letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; // alphebetical
for (var i = 0; i < letters.length; i ++) {
    keyCodeToLetter[i + 65] = letters[i]; // map letters to keys
} // mapping



// ARRAYS =======================================================
{  var dictionary = "which, there, their, about, would, these, other, words, could, write, first, water, after, where, right, think, three, years, place, sound, great, again, still, every, small, found, those, never, under, might, while, house, world, below, asked, going, large, until, along, shall, being, often, earth, began, since, study, night, light, above, paper, parts, young, story, point, times, heard, whole, white, given, means, music, miles, thing, today, later, using, money, lines, order, group, among, learn, known, space, table, early, trees, short, hands, state, black, shown, stood, front, voice, kinds, makes, comes, close, power, lived, vowel, taken, built, heart, ready, quite, class, bring, round, horse, shows, piece, green, stand, birds, start, river, tried, least, field, whose, girls, leave, added, color, third, hours, moved, plant, doing, names, forms, heavy, ideas, cried, check, floor, begin, woman, alone, plane, spell, watch, carry, wrote, clear, named, books, child, glass, human, takes, party, build, seems, blood, sides, seven, mouth, solve, north, value, death, maybe, happy, tells, gives, looks, shape, lives, steps, areas, sense, speak, force, ocean, speed, women, metal, south, grass, scale, cells, lower, sleep, wrong, pages, ships, needs, rocks, eight, major, level, total, ahead, reach, stars, store, sight, terms, catch, works, board, cover, songs, equal, stone, waves, guess, dance, spoke, break, cause, radio, weeks, lands, basic, liked, trade, fresh, final, fight, meant, drive, spent, local, waxes, knows, train, bread, homes, teeth, coast, thick, brown, clean, quiet, sugar, facts, steel, forth, rules, notes, units, peace, month, verbs, seeds, helps, sharp, visit, woods, chief, walls, cross, wings, grown, cases, foods, crops, fruit, stick, wants, stage, sheep, nouns, plain, drink, bones, apart, turns, moves, touch, angle, based, range, marks, tired, older, farms, spend, shoes, goods, chair, twice, cents, empty, alike, style, broke, pairs, count, enjoy, score, shore, roots, paint, heads, shook, serve, angry, crowd, wheel, quick, dress, share, alive, noise, solid, cloth, signs, hills, types, drawn, worth, truck, piano, upper, loved, usual, faces, drove, cabin, boats, towns, proud, court, model, prime, fifty, plans, yards, prove, tools, price, sheet, smell, boxes, raise, match, truth, roads, threw, enemy, lunch, chart, scene, graph, doubt, guide, winds, block, grain, smoke, mixed, games, wagon, sweet, topic, extra, plate, title, knife, fence, falls, cloud, wheat, plays, enter, broad, steam, atoms, press, lying, basis, clock, taste, grows, thank, storm, agree, brain, track, smile, funny, beach, stock, hurry, saved, sorry, giant, trail, offer, ought, rough, daily, avoid, keeps, throw, allow, cream, laugh, edges, teach, frame, bells, dream, magic, occur, ended, chord, false, skill, holes, dozen, brave, apple, climb, outer, pitch, ruler, holds, fixed, costs, calls, blank, staff, labor, eaten, youth, tones, honor, globe, gases, doors, poles, loose, apply, tears, exact, brush, chest, layer, whale, minor, faith, tests, judge, items, worry, waste, hoped, strip, begun, aside, lakes, bound, depth, candy, event, worse, aware, shell, rooms, ranch, image, snake, aloud, dried, likes, motor, pound, knees, refer, fully, chain, shirt, flour, drops, spite, orbit, banks, shoot, curve, tribe, tight, blind, slept, shade, claim, flies, theme, queen, fifth, union, hence, straw, entry, issue, birth, feels, anger, brief, rhyme, glory, guard, flows, flesh, owned, trick, yours, sizes, noted, width, burst, route, lungs, uncle, bears, royal, kings, forty, trial, cards, brass, opera, chose, owner, vapor, beats, mouse, tough, wires, meter, tower, finds, inner, stuck, arrow, poems, label, swing, solar, truly, tense, beans, split, rises, weigh, hotel, stems, pride, swung, grade, digit, badly, boots, pilot, sales, swept, lucky, prize, stove, tubes, acres, wound, steep, slide, trunk, error, porch, slave, exist, faced, mines, marry, juice, raced, waved, goose, trust, fewer, favor, mills, views, joint, eager, spots, blend, rings, adult, index, nails, horns, balls, flame, rates, drill, trace, skins, waxed, seats, stuff, ratio, minds, dirty, silly, coins, hello, trips, leads, rifle, hopes, bases, shine, bench, moral, fires, meals, shake, shops, cycle, movie, slope, canoe, teams, folks, fired, bands, thumb, shout, canal, habit, reply, ruled, fever, crust, shelf, walks, midst, crack, print, tales, coach, stiff, flood, verse, awake, rocky, march, fault, swift, faint, civil, ghost, feast, blade, limit, germs, reads, ducks, dairy, worst, gifts, lists, stops, rapid, brick, claws, beads, beast, skirt, cakes, lions, frogs, tries, nerve, grand, armed, treat, honey, moist, legal, penny, crown, shock, taxes, sixty, altar, pulls, sport, drums, talks, dying, dates, drank, blows, lever, wages, proof, drugs, tanks, sings, tails, pause, herds, arose, hated, clues, novel, shame, burnt, races, flash, weary, heels, token, coats, spare, shiny, alarm, dimes, sixth, clerk, mercy, sunny, guest, float, shone, pipes, worms, bills, sweat, suits, smart, upset, rains, sandy, rainy, parks, sadly, fancy, rider, unity, bunch, rolls, crash, craft, newly, gates, hatch, paths, funds, wider, grace, grave, tides, admit, shift, sails, pupil, tiger, angel, cruel, agent, drama, urged, patch, nests, vital, sword, blame, weeds, screw, vocal, bacon, chalk, cargo, crazy, acted, goats, arise, witch, loves, dwell, backs, ropes, shots, merry, phone, cheek, peaks, ideal, beard, eagle, creek, cries, ashes, stall, yield, mayor, opens, input, fleet, tooth, cubic, wives, burns, poets, apron, spear, organ, cliff, stamp, paste, rural, baked, chase, slice, slant, knock, noisy, sorts, stays, wiped, blown, piled, clubs, cheer, widow, twist, tenth, hides, comma, sweep, spoon, stern, crept, maple, deeds, rides, muddy, crime, jelly, ridge, drift, dusty, devil, tempo, humor, sends, steal, tents, waist, roses, reign, noble, cheap, dense, linen, geese, woven, posts, hired, wrath, salad, bowed, tires, shark, belts, grasp, blast, polar, fungi, tends, pearl, loads, jokes, veins, frost, hears, loses, hosts, diver, phase, toads, alert, tasks, seams, coral, focus, naked, puppy, jumps, spoil, quart, macro, fears, flung, spark, vivid, brook, steer, spray, decay, ports, socks, urban, goals, grant, minus, films, tunes, shaft, firms, skies, bride, wreck, flock, stare, hobby, bonds, dared, faded, thief, crude, pants, flute, votes, tonal, radar, wells, skull, hairs, argue, wears, dolls, voted, caves, cared, broom, scent, panel, fairy, olive, bends, prism, lamps, cable, peach, ruins, rally, schwa, lambs, sells, cools, draft, charm, limbs, brake, gazed, cubes, delay, beams, fetch, ranks, array, harsh, camel, vines, picks, naval, purse, rigid, crawl, toast, soils, sauce, basin, ponds, twins, wrist, fluid, pools, brand, stalk, robot, reeds, hoofs, buses, sheer, grief, bloom, dwelt, melts, risen, flags, knelt, fiber, roofs, freed, armor, piles, aimed, algae, twigs, lemon, ditch, drunk, rests, chill, slain, panic, cords, tuned, crisp, ledge, dived, swamp, clung, stole, molds, yarns, liver, gauge, breed, stool, gulls, awoke, gross, diary, rails, belly, trend, flask, stake, fried, draws, actor, handy, bowls, haste, scope, deals, knots, moons, essay, thump, hangs, bliss, dealt, gains, bombs, clown, palms, cones, roast, tidal, bored, chant, acids, dough, camps, swore, lover, hooks, males, cocoa, punch, award, reins, ninth, noses, links, drain, fills, nylon, lunar, pulse, flown, elbow, fatal, sites, moths, meats, foxes, mined, attic, fiery, mount, usage, swear, snowy, rusty, scare, traps, relax, react, valid, robin, cease, gills, prior, safer, polio, loyal, swell, salty, marsh, vague, weave, mound, seals, mules, virus, scout, acute, windy, stout, folds, seize, hilly, joins, pluck, stack, lords, dunes, burro, hawks, trout, feeds, scarf, halls, coals, towel, souls, elect, buggy, pumps, loans, spins, files, oxide, pains, photo, rival, flats, syrup, rodeo, sands, moose, pints, curly, comic, cloak, onion, clams, scrap, didst, couch, codes, fails, ounce, lodge, greet, gypsy, utter, paved, zones, fours, alley, tiles, bless, crest, elder, kills, yeast, erect, bugle, medal, roles, hound, snail, alter, ankle, relay, loops, zeros, bites, modes, debts, realm, glove, rayon, swims, poked, stray, lifts, maker, lumps, graze, dread, barns, docks, masts, pours, wharf, curse, plump, robes, seeks, cedar, curls, jolly, myths, cages, gloom, locks, pedal, beets, crows, anode, slash, creep, rowed, chips, fists, wines, cares, valve, newer, motel, ivory, necks, clamp, barge, blues, alien, frown, strap, crews, shack, gonna, saves, stump, ferry, idols, cooks, juicy, glare, carts, alloy, bulbs, lawns, lasts, fuels, oddly, crane, filed, weird, shawl, slips, troop, bolts, suite, sleek, quilt, tramp, blaze, atlas, odors, scrub, crabs, probe, logic, adobe, exile, rebel, grind, sting, spine, cling, desks, grove, leaps, prose, lofty, agony, snare, tusks, bulls, moods, humid, finer, dimly, plank, china, pines, guilt, sacks, brace, quote, lathe, gaily, fonts, scalp, adopt, foggy, ferns, grams, clump, perch, tumor, teens, crank, fable, hedge, genes, sober, boast, tract, cigar, unite, owing, thigh, haiku, swish, dikes, wedge, booth, eased, frail, cough, tombs, darts, forts, choir, pouch, pinch, hairy, buyer, torch, vigor, waltz, heats, herbs, users, flint, click, madam, bleak, blunt, aided, lacks, masks, waded, risks, nurse, chaos, sewed, cured, ample, lease, steak, sinks, merit, bluff, bathe, gleam, bonus, colts, shear, gland, silky, skate, birch, anvil, sleds, groan, maids, meets, speck, hymns, hints, drown, bosom, slick, quest, coils, spied, snows, stead, snack, plows, blond, tamed, thorn, waits, glued, banjo, tease, arena, bulky, carve, stunt, warms, shady, razor, folly, leafy, notch, fools, otter, pears, flush, genus, ached, fives, flaps, spout, smote, fumes, adapt, cuffs, tasty, stoop, clips, disks, sniff, lanes, brisk, imply, demon, super, furry, raged, growl, texts, hardy, stung, typed, hates, wiser, timid, serum, beaks, rotor, casts, baths, glide, plots, trait, resin, slums, lyric, puffs, decks, brood, mourn, aloft, abuse, whirl, edged, ovary, quack, heaps, slang, await, civic, saint, bevel, sonar, aunts, packs, froze, tonic, corps, swarm, frank, repay, gaunt, wired, niece, cello, needy, chuck, stony, media, surge, hurts, repel, husky, dated, hunts, mists, exert, dries, mates, sworn, baker, spice, oasis, boils, spurs, doves, sneak, paces, colon, siege, strum, drier, cacao, humus, bales, piped, nasty, rinse, boxer, shrub, amuse, tacks, cited, slung, delta, laden, larva, rents, yells, spool, spill, crush, jewel, snaps, stain, kicks, tying, slits, rated, eerie, smash, plums, zebra, earns, bushy, scary, squad, tutor, silks, slabs, bumps, evils, fangs, snout, peril, pivot, yacht, lobby, jeans, grins, viola, liner, comet, scars, chops, raids, eater, slate, skips, soles, misty, urine, knobs, sleet, holly, pests, forks, grill, trays, pails, borne, tenor, wares, carol, woody, canon, wakes, kitty, miner, polls, shaky, nasal, scorn, chess, taxis, crate, shyly, tulip, forge, nymph, budge, lowly, abide, depot, oases, asses, sheds, fudge, pills, rivet, thine, groom, lanky, boost, broth, heave, gravy, beech, timed, quail, inert, gears, chick, hinge, trash, clash, sighs, renew, bough, dwarf, slows, quill, shave, spore, sixes, chunk, madly, paced, braid, fuzzy, motto, spies, slack, mucus, magma, awful, discs, erase, posed, asset, cider, taper, theft, churn, satin, slots, taxed, bully, sloth, shale, tread, raked, curds, manor, aisle, bulge, loins, stair, tapes, leans, bunks, squat, towed, lance, panes, sakes, heirs, caste, dummy, pores, fauna, crook, poise, epoch, risky, warns, fling, berry, grape, flank, drags, squid, pelts, icing, irony, irons, barks, whoop, choke, diets, whips, tally, dozed, twine, kites, bikes, ticks, riots, roars, vault, looms, scold, blink, dandy, pupae, sieve, spike, ducts, lends, pizza, brink, widen, plumb, pagan, feats, bison, soggy, scoop, argon, nudge, skiff, amber, sexes, rouse, salts, hitch, exalt, leash, dined, chute, snort, gusts, melon, cheat, reefs, llama, lasso, debut, quota, oaths, prone, mixes, rafts, dives, stale, inlet, flick, pinto, brows, untie, batch, greed, chore, stirs, blush, onset, barbs, volts, beige, swoop, paddy, laced, shove, jerky, poppy, leaks, fares, dodge, godly, squaw, affix, brute, nicer, undue, snarl, merge, doses, showy, daddy, roost, vases, swirl, petty, colds, curry, cobra, flare, messy, cores, soaks, ripen, whine, amino, plaid, spiny, mowed, baton, peers, vowed, pious, swans, exits, afoot, plugs, idiom, chili, rites, serfs, cleft, berth, grubs, annex, dizzy, hasty, latch, wasps, mirth, baron, plead, aloof, aging, pixel, bared, mummy, hotly, auger, buddy, chaps, badge, stark, fairs, gully, mumps, emery, filly, ovens, drone, gauze, idiot, fussy, annoy, shank, gouge, bleed, elves, roped, unfit, baggy, mower, scant, grabs, fleas, lousy, album, sawed, cooky, murky, infer, burly, waged, dingy, brine, kneel, creak, vanes, smoky, spurt, combs, easel, laces, humps, rumor, aroma, horde, swiss, leapt, opium, slime, afire, pansy, mares, soaps, husks, snips, hazel, lined, cafes, naive, wraps, sized, piers, beset, agile, tongs, steed, fraud, booty, valor, downy, witty, mossy, psalm, scuba, tours, polka, milky, gaudy, shrug, tufts, wilds, laser, truss, hares, creed, lilac, siren, tarry, bribe, swine, muted, flips, cures, sinew, boxed, hoops, gasps, hoods, niche, yucca, glows, sewer, whack, fuses, gowns, droop, bucks, pangs, mails, whisk, haven, clasp, sling, stint, urges, champ, piety, chirp, pleat, posse, sunup, menus, howls, quake, knack, plaza, fiend, caked, bangs, erupt, poker, olden, cramp, voter, poses, manly, slump, fined, grips, gaped, purge, hiked, maize, fluff, strut, sloop, prowl, roach, cocks, bland, dials, plume, slaps, soups, dully, wills, foams, solos, skier, eaves, totem, fused, latex, veils, mused, mains, myrrh, racks, galls, gnats, bouts, sisal, shuts, hoses, dryly, hover, gloss, seeps, denim, putty, guppy, leaky, dusky, filth, oboes, spans, fowls, adorn, glaze, haunt, dares, obeys, bakes, abyss, smelt, gangs, aches, trawl, claps, undid, spicy, hoist, fades, vicar, acorn, pussy, gruff, musty, tarts, snuff, hunch, truce, tweed, dryer, loser, sheaf, moles, lapse, tawny, vexed, autos, wager, domes, sheen, clang, spade, sowed, broil, slyly, studs, grunt, donor, slugs, aspen, homer, croak, tithe, halts, avert, havoc, hogan, glint, ruddy, jeeps, flaky, ladle, taunt, snore, fines, props, prune, pesos, radii, pokes, tiled, daisy, heron, villa, farce, binds, cites, fixes, jerks, livid, waked, inked, booms, chews, licks, hyena, scoff, lusty, sonic, smith, usher, tucks, vigil, molts, sects, spars, dumps, scaly, wisps, sores, mince, panda, flier, axles, plied, patio, rabbi, petal, polyp, tints, grate, troll, tolls, relic, phony, bleat, flaws, flake, snags, aptly, drawl, ulcer, soapy, bossy, monks, crags, caged, twang, diner, taped, cadet, grids, spawn, guile, noose, mores, girth, slimy, aides, spasm, burrs, alibi, lymph, saucy, muggy, liter, joked, goofy, exams, enact, stork, lured, toxic, omens, nears, covet, wrung, forum, venom, moody, alder, sassy, flair, guild, prays, wrens, hauls, stave, tilts, pecks, stomp, gales, tempt, capes, mesas, omits, tepee, harry, wring, evoke, limes, cluck, lunge, highs, canes, giddy, lithe, verge, khaki, queue, loath, foyer, outdo, fared, deter, crumb, astir, spire, jumpy, extol, buoys, stubs, lucid, thong, afore, whiff, maxim, hulls, clogs, slats, jiffy, arbor, cinch, igloo, goody, gazes, dowel, calms, bitch, scowl, gulps, coded, waver, mason, lobes, ebony, flail, isles, clods, dazed, adept, oozed, sedan, clays, warts, ketch, skunk, manes, adore, sneer, mango, fiord, flora, roomy, minks, thaws, watts, freer, exult, plush, paled, twain, clink, scamp, pawed, grope, bravo, gable, stink, sever, waned, rarer, regal, wards, fawns, babes, unify, amend, oaken, glade, visor, hefty, nines, throb, pecan, pence, sills, jails, flyer, saber, nomad, miter, beeps, domed, gulfs, curbs, heath, moors, aorta, larks, tangy, wryly, cheep, rages, evade, lures, freak, vogue, tunic, slams, knits, dumpy, mania, spits, firth, hikes, trots, nosed, clank, dogma, bloat, balsa, graft, middy, stile, keyed, finch, chaff, wiles, amigo, copra, amiss, eying, twirl, lurch, popes, chins, smock, tines, guise, grits, junks, shoal, cache, tapir, atoll, deity, toils, spree, mocks, scans, shorn, revel, raven, hoary, reels, scuff, mimic, weedy, corny, truer, rouge, ember, floes, torso, wipes, edict, sulky, recur, groin, baste, kinks, surer, piggy, moldy, franc, liars, inept, gusty, facet, jetty, equip, leper, slink, soars, cater, dowry, sided, yearn, decoy, taboo, ovals, heals, pleas, beret, spilt, rover, endow, pygmy, carat, abbey, vents, waken, chimp, fumed, sodas, vinyl, clout, wades, mites, smirk, bores, bunny, surly, frock, foray, purer, milks, query, mired, blare, froth, gruel, navel, paler, puffy, casks, grime, derby, mamma, gavel, teddy, vomit, moans, allot, defer, wield, viper, louse, erred, hewed, abhor, wrest, waxen, adage, ardor, stabs, pored, rondo, loped, fishy, bible, hires, foals, feuds, jambs, thuds, jeers, knead, quirk, rugby, expel, greys, rigor, ester, lyres, aback, glues, lotus, lurid, rungs, hutch, thyme, valet, tommy, yokes, epics, trill, pikes, ozone, caper, chime, frees, famed, leech, smite, neigh, erode, robed, hoard, salve, conic, gawky, craze, jacks, gloat, mushy, rumps, fetus, wince, pinks, shalt, toots, glens, cooed, rusts, stews, shred, parka, chugs, winks, clots, shrew, booed, filmy, juror, dents, gummy, grays, hooky, butte, dogie, poled, reams, fifes, spank, tepid, spook, taint, rogue, spiky, opals, miser, cocky, coyly, balmy, slosh, brawl, aphid, faked, hydra, brags, chide, yanks, allay, video, altos, eases, meted, chasm, longs, excel, taffy, impel, savor, koala, quays, dawns, proxy, clove, duets, dregs, tardy, briar, grimy, ultra, meaty, halve, wails, suede, mauve, envoy, arson, coves, gooey, brews, sofas, chums, amaze, zooms, abbot, halos, scour, suing, cribs, sagas, enema, wordy, harps, coupe, molar, flops, weeps, mints, ashen, felts, askew, munch, mewed, divan, vices, jumbo, blobs, blots, spunk, acrid, topaz, cubed, clans, flees, slurs, gnaws, welds, fords, emits, agate, pumas, mends, darks, dukes, plies, canny, hoots, oozes, lamed, fouls, clefs, nicks, mated, skims, brunt, tuber, tinge, fates, ditty, thins, frets, eider, bayou, mulch, fasts, amass, damps, morns, friar, palsy, vista, croon, conch, udder, tacos, skits, mikes, quits, preen, aster, adder, elegy, pulpy, scows, baled, hovel, lavas, crave, optic, welts, busts, knave, razed, shins, totes, scoot, dears, crock, mutes, trims, skein, doted, shuns, veers, fakes, yoked, wooed, hacks, sprig, wands, lulls, seers, snobs, nooks, pined, perky, mooed, frill, dines, booze, tripe, prong, drips, odder, levee, antic, sidle, pithy, corks, yelps, joker, fleck, buffs, scram, tiers, bogey, doled, irate, vales, coped, hails, elude, bulks, aired, vying, stags, strew, cocci, pacts, scabs, silos, dusts, yodel, terse, jaded, baser, jibes, foils, sways, forgo, slays, preys, treks, quell, peeks, assay, lurks, eject, boars, trite, belch, gnash, wanes, lutes, whims, dosed, chewy, snipe, umbra, teems, dozes, kelps, upped, brawn, doped, shush, rinds, slush, moron, voile, woken, fjord, sheik, jests, kayak, slews, toted, saner, drape, patty, raves, sulfa, grist, skied, vixen, civet, vouch, tiara, homey, moped, runts, serge, kinky, rills, corns, brats, pries, amble, fries, loons, tsars, datum, musky, pigmy, gnome, ravel, ovule, icily, liken, lemur, frays, silts, sifts, plods, ramps, tress, earls, dudes, waive, karat, jolts, peons, beers, horny, pales, wreak, lairs, lynch, stank, swoon, idler, abort, blitz, ensue, atone, bingo, roves, kilts, scald, adios, cynic, dulls, memos, elfin, dales, peels, peals, bares, sinus, crone, sable, hinds, shirk, enrol, wilts, roams, duped, cysts, mitts, safes, spats, coops, filet, knell, refit, covey, punks, kilns, fitly, abate, talcs, heeds, duels, wanly, ruffs, gauss, lapel, jaunt, whelp, cleat, gauzy, dirge, edits, wormy, moats, smear, prods, bowel, frisk, vests, bayed, rasps, tames, delve, embed, befit, wafer, ceded, novas, feign, spews, larch, huffs, doles, mamas, hulks, pried, brims, irked, aspic, swipe, mealy, skimp, bluer, slake, dowdy, penis, brays, pupas, egret, flunk, phlox, gripe, peony, douse, blurs, darns, slunk, lefts, chats, inane, vials, stilt, rinks, woofs, wowed, bongs, frond, ingot, evict, singe, shyer, flied, slops, dolts, drool, dells, whelk, hippy, feted, ether, cocos, hives, jibed, mazes, trios, sirup, squab, laths, leers, pasta, rifts, lopes, alias, whirs, diced, slags, lodes, foxed, idled, prows, plait, malts, chafe, cower, toyed, chefs, keels, sties, racer, etude, sucks, sulks, micas, czars, copse, ailed, abler, rabid, golds, croup, snaky, visas, palls, mopes, boned, wispy, raved, swaps, junky, doily, pawns, tamer, poach, baits, damns, gumbo, daunt, prank, hunks, buxom, heres, honks, stows, unbar, idles, routs, sages, goads, remit, copes, deign, culls, girds, haves, lucks, stunk, dodos, shams, snubs, icons, usurp, dooms, hells, soled, comas, paves, maths, perks, limps, wombs, blurb, daubs, cokes, sours, stuns, cased, musts, coeds, cowed, aping, zoned, rummy, fetes, skulk, quaff, rajah, deans, reaps, galas, tills, roved, kudos, toned, pared, scull, vexes, punts, snoop, bails, dames, hazes, lores, marts, voids, ameba, rakes, adzes, harms, rears, satyr, swill, hexes, colic, leeks, hurls, yowls, ivies, plops, musks, papaw, jells, bused, cruet, bided, filch, zests, rooks, laxly, rends, loams, basks, sires, carps, pokey, flits, muses, bawls, shuck, viler, lisps, peeps, sorer, lolls, prude, diked, floss, flogs, scums, dopes, bogie, pinky, leafs, tubas, scads, lowed, yeses, biked, qualm, evens, caned, gawks, whits, wooly, gluts, romps, bests, dunce, crony, joist, tunas, boner, malls, parch, avers, crams, pares, dally, bigot, kales, flays, leach, gushy, pooch, huger, slyer, golfs, mires, flues, loafs, arced, acnes, neons, fiefs, dints, dazes, pouts, cored, yules, lilts, beefs, mutts, fells, cowls, spuds, lames, jawed, dupes, deads, bylaw, noons, nifty, clued, vireo, gapes, metes, cuter, maims, droll, cupid, mauls, sedge, papas, wheys, eking, loots, hilts, meows, beaus, dices, peppy, riper, fogey, gists, yogas, gilts, skews, cedes, zeals, alums, okays, elope, grump, wafts, soots, blimp, hefts, mulls, hosed, cress, doffs, ruder, pixie, waifs, ousts, pucks, biers, gulch, suets, hobos, lints, brans, teals, garbs, pewee, helms, turfs, quips, wends, banes, napes, icier, swats, bagel, hexed, ogres, goner, gilds, pyres, lards, bides, paged, talon, flout, medic, veals, putts, dirks, dotes, tippy, blurt, piths, acing, barer, whets, gaits, wools, dunks, heros, swabs, dirts, jutes, hemps, surfs, okapi, chows, shoos, dusks, parry, decal, furls, cilia, sears, novae, murks, warps, slues, lamer, saris, weans, purrs, dills, togas, newts, meany, bunts, razes, goons, wicks, ruses, vends, geode, drake, judos, lofts, pulps, lauds, mucks, vises, mocha, oiled, roman, ethyl, gotta, fugue, smack, gourd, bumpy, radix, fatty, borax, cubit, cacti, gamma, focal, avail, papal, golly, elite, billy, adieu, annum, howdy, rhino, norms, bobby, axiom, setup, yolks, terns, mixer, genre, knoll, abode, junta, gorge, combo, alpha, overt, kinda, spelt, prick, nobly, ephod, audio, modal, veldt, warty, fluke, bonny, bream, rosin, bolls, doers, downs, beady, motif, humph, fella, mould, crepe, kerns, aloha, glyph, azure, riser, blest, locus, lumpy, beryl, wanna, brier, tuner, rowdy, mural, timer, canst, krill, quoth, triad, tenon, amply, deeps, padre, leant, pacer, octal, dolly, trans, sumac, foamy, lolly, giver, quipu, codex, manna, unwed, vodka, ferny, salon, duple, boron, revue, crier, alack, inter, dilly, whist, cults, spake, reset, loess, decor, mover, verve, ethic, gamut, lingo, dunno, align, sissy, incur, reedy, avant, piper, waxer, calyx, basil, coons, seine, piney, lemma, trams, winch, whirr, saith, ionic, heady, harem, tummy, sally, shied, dross, farad, saver, tilde, jingo, bower, serif, facto, belle, inset, bogus, caved, forte, sooty, bongo, toves, credo, basal, aglow, glean, gusto, hymen, ethos, terra, brash, scrip, swash, aleph, tinny, itchy, trice, jowls, gongs, twill, sower, henry, awash, libel, spurn, sabre, rebut, penal, obese, sonny, quirt, tacit, greek, xenon, hullo, pique, roger, negro, hadst, gecko, beget, uncut, aloes, quint, clunk, raped, salvo, diode, matey, hertz, xylem, kiosk, apace, cawed, peter, wench, cohos, sorta, gamba, bytes, tango, nutty, axial, aleck, natal, clomp, gored, siree, bandy, gunny, runic, whizz, rupee, fated, wiper, bards, briny, staid, hocks, ochre, yummy, gents, soupy, roper, swath, cameo, edger, spate, gimme, ebbed, breve, theta, deems, dykes, servo, telly, tabby, tares, blocs, welch, ghoul, vitae, cumin, dinky, bronc, tabor, teeny, comer, borer, sired, privy, mammy, deary, gyros, sprit, conga, quire, thugs, furor, bloke, runes, cadre, toxin, annul, egged, anion, nodes, picky, stein, jello, audit, echos, fagot, letup, eyrie, fount, caped, axons, amuck, banal, riled, petit, umber, miler, fibre, agave, bated, bilge, vitro, feint, pudgy, mater, manic, umped, pesky, strep, slurp, pylon, puree, caret, temps, newel, yawns, seedy, treed, coups, rangy, brads, mangy, loner, circa, tibia, afoul, mommy, titer, carne, kooky, motes, amity, suave, hippo, curvy, samba, newsy, anise, imams, tulle, aways, liven, hallo, wales, opted, canto, idyll, bodes, curio, wrack, hiker, chive, yokel, dotty, demur, cusps, specs, quads, laity, toner, decry, writs, saute, clack, aught, logos, tipsy, natty, ducal, bidet, bulgy, metre, lusts, unary, baler, sited, shies, hasps, brung, holed, swank, looky, melee, huffy, loamy, titan, binge, shunt, femur, libra, seder, honed, annas, coypu, shims, zowie, jihad, savvy, nadir, basso, monic, maned, mousy, omega, laver, prima, picas, folio, mecca, reals, troth, testy, balky, crimp, chink, abets, splat, abaci, vaunt, cutie, pasty, moray, levis, ratty, islet, joust, motet, viral, nukes, grads, comfy, voila, woozy, blued, whomp, sward, metro, skeet, chine, aerie, bowie, tubby, emirs, coati, unzip, slobs, trike, funky, ducat, dewey, skoal, wadis, oomph, taker, minim, getup, stoic, synod, runty, flyby, braze, inlay, venue, louts, peaty, orlon, humpy, radon, beaut, raspy, unfed, crick, nappy, vizor, yipes, rebus, divot, kiwis, vetch, squib, sitar, kiddo, dyers, cotta, matzo, lager, zebus, crass, dacha, kneed, dicta, fakir, knurl, runny, unpin, julep, globs, nudes, sushi, tacky, stoke, kaput, butch, hulas, croft, achoo, genii, nodal, outgo, spiel, viols, fetid, cagey, fudgy, epoxy, leggy, hanky, lapis, felon, beefy, coots, melba, caddy, segue, betel, frizz, drear, kooks, turbo, hoagy, moult, helix, zonal, arias, nosey, paean, lacey, banns, swain, fryer, retch, tenet, gigas, whiny, ogled, rumen, begot, cruse, abuts, riven, balks, sines, sigma, abase, ennui, gores, unset, augur, sated, odium, latin, dings, moire, scion, henna, kraut, dicks, lifer, prigs, bebop, gages, gazer, fanny, gibes, aural, tempi, hooch, rapes, snuck, harts, techs, emend, ninny, guava, scarp, liege, tufty, sepia, tomes, carob, emcee, prams, poser, verso, joule, baize, blips, scrim, cubby, clave, winos, rearm, liens, lumen, chump, nanny, trump, fichu, chomp, homos, maser, woosh, patsy, shill, rusks, avast, swami, boded, lobed, natch, shish, tansy, snoot, payer, altho, sappy, laxer, hubby, aegis, riles, ditto, jazzy, dingo, quasi, septa, peaky, lorry, heerd, bitty, payee, seamy, apses, imbue, belie, chary, spoof, phyla, clime, babel, wacky, sumps, skids, khans, crypt, inure, nonce, outen, hooey, anole, kazoo, calve, limbo, argot, ducky, faker, vibes, gassy, unlit, nervy, femme, biter, fiche, boors, gaffe, saxes, recap, synch, dicey, ouija, hewer, legit, gurus, edify, tweak, typos, rerun, polly, surds, hamza, nulls, hater, lefty, mogul, mafia, debug, pates, blabs, splay, talus, moola, nixed, kilos, snide, horsy, gesso, jaggy, trove, nixes, creel, pater, iotas, cadge, skyed, hokum, furze, ankhs, curie, nutsy, hilum, remix, angst, burls, jimmy, veiny, tryst, codon, befog, gamed, flume, axman, doozy, lubes, rheas, bozos, butyl, kelly, mynah, jocks, donut, avian, wurst, chock, quash, quals, hayed, bombe, cushy, spacy, puked, leery, thews, prink, amens, tesla, intro, fiver, frump, capos, opine, coder, namer, jowly, pukes, haled, chard, duffs, bruin, reuse, whang, toons, frats, silty, telex, cutup, nisei, neato, decaf, softy, bimbo, adlib, loony, shoed, agues, peeve, noway, gamey, sarge, reran, epact, potty, coned, upend, narco, ikats, whorl, jinks, tizzy, weepy, posit, marge, vegan, clops, numbs, reeks, rubes, rower, biped, tiffs, hocus, hammy, bunco, tykes, chaws, yucky, hokey, resew, maven, adman, slogs, souse, nacho, mimed, melds, boffo, debit, pinup, vagus, gulag, randy, bosun, educe, faxes, auras, pesto, antsy, betas, fizzy, dorky, snits, moxie, thane, mylar, nobby, gamin, gouty, esses, goyim, paned, druid, jades, rehab, gofer, tzars, octet, homed, socko, dorks, eared, anted, elide, fazes, oxbow, dowse, situs, macaw, scone, drily, hyper, salsa, mooch, gated, unjam, lipid, mitre, venal, knish, ritzy, divas, torus, mange, dimer, recut, meson, wined, fends, phage, fiats, caulk, cavil, panty, roans, bilks, hones, botch, estop, sully, sooth, gelds, ahold, raper, pager, fixer, infix, tuxes, plebe, twits, abash, twixt, wacko, primp, nabla, girts, miffs, emote, xerox, rebid, shahs, rutty, grout, grift, deify, biddy, kopek, semis, bries, acmes, piton, hussy, torts, disco, whore, boozy, gibed, vamps, amour, soppy, gonzo, durst, wader, tutus, perms, catty, glitz, brigs, nerds, barmy, gizmo, owlet, sayer, molls, shard, whops, comps, corer, colas, matte, droid, ploys, vapid, cairn, deism, mixup, yikes, prosy, raker, flubs, whish, reify, craps, shags, clone, hazed, macho, recto, refix, drams, biker, aquas, porky, doyen, exude, goofs, divvy, noels, jived, hulky, cager, harpy, oldie, admix, codas, zilch, deist, orcas, retro, pilaf, parse, rants, zingy, toddy, chiff, micro, veeps, girly, nexus, demos, antes, lulus, gnarl, zippy, ivied, epees, wimps, tromp, grail, yoyos, poufs, hales, roust, cabal, rawer, pampa, mosey, kefir, burgs, unmet, boobs, boons, hypes, dynes, nards, lanai, yogis, sepal, quark, toked, prate, ayins, hawed, swigs, doper, bossa, linty, foist, mondo, stash, kayos, twerp, zesty, capon, wimpy, rewed, fungo, tarot, frosh, kabob, pinko, redid, mimeo, heist, tarps, lamas, sutra, dinar, whams, busty, spays, mambo, nabob, preps, odour, cabby, conks, sluff, dados, houri, swart, balms, gutsy, faxed, egads, pushy, retry, agora, drubs, daffy, chits, mufti, karma, lotto, toffs, burps, deuce, zings, kappa, clads, doggy, duper, scams, ogler, mimes, throe, zetas, waled, promo, blats, muffs, oinks, viand, coset, finks, faddy, minis, snafu, sauna, usury, muxes, craws, stats, condo, coxes, loopy, dorms, ascot, dippy, execs, dopey, envoi, umpty, gismo, fazed, strop, jives, slims, batik, pings, sonly, pekoe, prawn, luaus, campy, oodle, prexy, proms, touts, ogles, tweet, toady, naiad, hider, nuked, fatso, sluts, obits, narcs, tyros, delis, wooer, hyped, byway, texas, scrod, avows, futon, torte, carom, kebab, tamps, jilts, duals, artsy, repro, modem, toped, psych, sicko, klutz, tarns, coxed, drays, cloys, piker, aimer, suras, limos, flack, hapax, dutch, mucky, shire, klieg, staph, layup, tokes, axing, toper, duvet, cowry, profs, blahs, addle, sudsy, batty, coifs, suety, gabby, pitas, gouda, deice, taupe, topes, duchy, nitro, carny, limey, orals, hirer, taxer, roils, ruble, elate, dolor, wryer, snots, quais, coked, gimel, gorse, minas, agape, manta, jings, iliac, admen, offen, offal, bolas, thwap, alway, boggy, donna, locos, belay, gluey, bitsy, hilar, outta, vroom, fetal, raths, renal, dyads, crocs, vires, culpa, kivas, feist, teats, yawls, whens, abaca, aphis, fusty, eclat, perdu, mayst, exeat, molly, supra, wetly, plasm, buffa, semen, pukka, tagua, paras, stoat, secco, carte, haute, molal, shads, forma, ovoid, pions, modus, rheum, scurf, parer, ephah, doest, sprue, flams, molto, miked, bronx, goopy, bally, plumy, moony, morts, yourn, bipod, spume, algal, ambit, mucho, dozer, groat, skint, laude, thrum, pappy, oncet, rimed, gigue, limed, plein, redly, lites, grebe, absit, pshaw, yawps, plats, payed, areal, tilth, youse, gwine, lento, spitz, yawed, gipsy, sprat, cornu, amahs, blowy, wahoo, lubra, coqui, sabra, edema, dicot, astro, kited, ouzel, didos, bonne, axmen, klunk, summa, laves, purls, yawny, teary, masse, largo, sylph, lulab, toque, plunk, ortho, lucre, cooch, folky, tyres, corky, injun, solon, didot, kerfs, rayed, chile, begat, nippy, litre, magna, rebox, hydro, milch, brent, gyves, lazed, feued, mavis, inapt, baulk, casus, scrum, wised, fossa, dower, kyrie, scuse, feuar, ohmic, juste, ukase, beaux, tusky, orate, musta, lardy, intra, quiff, epsom, neath, ocher, tared, homme, mezzo, corms, psoas, beaky, terry, infra, spivs, tuans, belli, bergs, anima, weirs, mahua, scops, manse, titre, curia, kebob, cycad, talky, fucks, tapis, amide, dolce, sloes, jakes, russe, blash, tutti, pruta, panga, blebs, tench, swarf, herem, missy, merse, pawky, limen, vivre, chert, unsee, tiros, brack, foots, welsh, fosse, knops, ileum, noire, firma, podgy, laird, thunk, shute, rowan, shoji, poesy, uncap, fames, glees, costa, turps, fores, solum, imago, byres, fondu, coney, polis, dictu, kraal, sherd, mumbo, wroth, chars, unbox, vacuo, slued, weest, hades, wiled, syncs, muser, excon, hoars, sibyl, passe, joeys, lepta, shays, bocks, endue, darer, nones, ileus, plash, busby, wheal, buffo, yobbo, biles, poxes, rooty, licit, terce, bromo, dweeb, imbed, saran, bruit, punky, softs, biffs, loppy, agars, aquae, livre, biome, bunds, shews, ginny, degum, polos, desex, unman, dungy, wedgy, glebe, apers, ridgy, roids, wifey, vapes, whoas, bunko, yolky, ulnas, reeky, bodge, brant, davit, deque, liker, jenny, tacts, fulls, ligne, refry, vower, aargh, churl, momma, gaols, whump, arras, marls, tiler, grogs, memes, midis, tided, haler, duces, twiny, poste, unrig, prise, drabs, quids, facer, spier, baric, geoid, remap, trier, gunks, steno, stoma, airer, ovate, torah, apian, smuts, pocks, yurts, exurb, defog, nuder, bosky, nimbi, mothy, joyed, labia, pards, jammy, bigly, faxer, hoppy, nurbs, cotes, dishy, vised, celeb, pismo, casas, dodgy, scudi, muons, ureas, ioctl, unhip, krone, sager, verst, expat, gronk, uvula, shawm, bilgy, braes, cento, webby, lippy, gamic, lordy, mazed, tings, shoat, faery, wirer, diazo, carer, rater, rente, zloty, viers, unapt, kepis, taxon, eyers, wonts, spina, stoae, yenta, buret, japan, bedew, hafts, selfs, oared, herby, pryer, oakum, dinks, titty, sepoy, penes, fusee, winey, gimps, nihil, rille, giber, ousel, umiak, cuppy, hames, shits, azine, glads, tacet, bumph, coyer, honky, gamer, waspy, sedgy, bents, varia, djinn, junco, pubic, wilco, lazes, idyls, lupus, rives, snood, schmo, finis, noter, pavan, orbed, bates, pipet, baddy, goers, shako, stets, sebum, seeth, lobar, raver, ajuga, riced, velds, dribs, ville, dhows, unsew, halma, krona, limby, jiffs, treys, bauds, mimer, plebs, caner, jiber, cuppa, washy, chuff, unarm, yukky, styes, waker, flaks, maces, rimes, gimpy, guano, liras, kapok, scuds, bwana, oring, aider, prier, klugy, monte, golem, velar, firer, pieta, umbel, campo, unpeg, fovea, abeam, boson, asker, goths, vocab, vined, trows, tikis, loper, indie, boffs, spang, grapy, tater, ichor, kilty, lochs, supes, degas, flics, torsi, beths, weber, resaw, lawny, coven, mujik, relet, therm, heigh, trued, zayin, liest, barfs, bassi, qophs, roily, flabs, punny, okras, hanks, dipso, nerfs, fauns, calla, pseud, lurer, magus, obeah, atria, twink, palmy, pocky, pends, recta, plonk, slaws, keens, nicad, pones, inker, whews, groks, mosts, trews, ulnar, gyppy, cocas, expos, eruct, oiler, vacua, dreck, dater, arums, tubal, voxel, dixit, beery, assai, lades, actin, buzzy, meads, grody, ribby, clews, creme, email, pyxie, kulak, bocci, rived, duddy, hoper, lapin, wonks, petri, phial, fugal, holon, boomy, duomo, musos, shier, hayer, porgy, hived, litho, fisty, stagy, maria, smogs, asana, yogic, slomo, fawny, amine, wefts, gonad, twirp, brava, plyer, fermi, loges, niter, revet, unate, gyved, totty, zappy, honer, giros, dicer, calks, luxes, monad, cruft, quoin, fumer, amped, shlep, vinca, yahoo, vulva, zooey, dryad, nixie, moper, iambs, lunes, nudie, limns, weals, nohow, miaow, gouts, mynas, mazer, kikes, oxeye, stoup, jujus, debar, pubes, taels, defun, rands, blear, paver, goosy, sprog, oleos, toffy, maced, crits, kluge, tubed, sahib, ganef, scats, sputa, vaned, acned, taxol, plink, oweth, tribs, resay, boule, thous, haply, glans, maxis, bezel, antis, porks, quoit, alkyd, glary, beamy, hexad, bonks, tecum, kerbs, filar, frier, redux, abuzz, fader, shoer, couth, trues, guyed, goony, booky, fuzes, hurly, genet, hodad, calix, filer, pawls, iodic, utero, henge, unsay, liers, piing, weald, sexed, folic, poxed, cunts, anile, kiths, becks, tatty, plena, rebar, abled, toyer, attar, teaks, aioli, awing, anent, feces, redip, wists, prats, mesne, muter, smurf, bahts, lossy, ftped, hunky, hoers, slier, sicks, fatly, delft, hiver, himbo, pengo, busks, loxes, zonks, ilium, aport, ikons, mulct, reeve, civvy, canna, barfy, scudo, knout, gaper, bhang, pease, uteri, lases, paten, axels, stoas, ombre, styli, gunky, hazer, kenaf, ahoys, ammos, weeny, urger, kudzu, paren, bolos, fetor, nitty, techy, lieth, somas, darky, villi, gluon, janes, cants, farts, socle, jinns, ruing, slily, ricer, hadda, wowee, rices, nerts, cauls, swive, micks, arity, pasha, oinky, gutty, tetra, wises, wolds, balds, picot, whats, shiki, bungs, snarf, legos, dungs, stogy, berms, tangs, vails, roods, morel, sware, elans, latus, gules, razer, doxie, buena, overs, gutta, zincs, nates, kirks, tikes, donee, jerry, mohel, ceder, doges, unmap, folia, rawly, snark, topoi, ceils, immix, yores, diest, bubba, pomps, forky, lawzy, poohs, worts, gloms, beano, muley, barky, tunny, auric, funks, gaffs, cordy, curdy, lisle, toric, soyas, reman, carpy, apish, oaten, gappy, aurae, bract, rooky, axled, burry, sizer, proem, turfy, mashy, miens, olios, grook, sates, agley, corgi, dashy, doser, dildo, xored, laker, playa, selah, malty, dulse, frigs, demit, whoso, rials, sawer, spics, bedim, snugs, fanin, azoic, icers, suers, wizen, koine, topos, shirr, rifer, feral, laded, lased, turds, swede, easts, cozen, unhit, pally, aitch, sedum, coper, ruche, geeks, swags, etext, algin, offed, ninja, holer, doter, toter, besot, macer, peens, pewit, redox, poler, yecch, fluky, doeth, twats, cruds, bebug, bider, stele, hexer, wests, gluer, pilau, abaft, whelm, lacer, inode, tabus, gator, cuing, refly, luted, cukes, bairn, bight, arses, crump, loggy, blini, spoor, toyon, harks, wazoo, fenny, naves, keyer, tufas, morph, rajas, typal, spiff, oxlip, unban, mussy, finny, rimer, login, molas, cirri, huzza, agone, unsex, unwon, peats, toile, zombi, dewed, nooky, alkyl, ixnay, dovey, holey, cuber, amyls, podia, chino, apnea, prims, lycra, johns, primo, fatwa, egger, hempy, snook, hying, fuzed, barms, crink, moots, yerba, rhumb, direr, munge, eland, nares, wrier, noddy, atilt, jukes, ender, unfix, doggo, zooks, diddy, shmoo, brusk, prest, curer, pasts, kelpy, bocce, kicky, taros, lings, dicky, nerdy, abend, stela, laved, baldy, pubis, gooks, wonky, stied, hypos, assed, spumy, osier, roble, rumba, biffy, pupal, jesus, ramen, latte, paleo, dijon, kamut, latke, queso, bento, satay, mochi, savoy".split(", ");
} //the DICTIONARY words
{  var theWords = "Ramen, Fruit, Onion, Beans, Peach, Apple, Melon, Pears, Grape, Plums, Lemon, Kiwis, Mango, Guava, Berry, Beets, Leeks, Cream, Sugar, Bread, Toast, Cakes, Wafer, Candy, Honey, Cocoa, Wheat, Bagel, Donut, Limes, Herbs, Olive, Fries, Pizza, Tacos, Ranch, Bacon, Rolls, Soups, Chips, Flour, Jelly, Juice, Sauce, Chili, Spice, Broth, Syrup, Sushi, Roast, Steak, Curry, Cumin, Basil, Thyme, Cider, Carob, Spelt, Pasta, Dough, Gourd, Mocha, Latte, Pilaf, Dates, Pecan, Gravy, Salad, Crepe, Salsa, Kabob, Scone, Tarts, Chive, Pinto, Prune, Taffy, Fudge, Chard, Pesto, Clams, Jello, Jerky, Anise, Cress, Torte, Squid, Mochi, Patty, Fungi, Bento, Bison, Brine, Curds, Crust, Crisp, Cukes, Glaze, Grits, Gummy, Icing, Kefir, Nacho, Mints, Punch, Queso, Seeds, Shake, Spuds, Trout, Quail, Salts, Yeast, Latke, Gyros, Wings, Cacao, Kamut, Yolks, Meats, Grain".split(", ");
} // list of THE WORDS FEATURED in this game (length = 11^2)
{  var alphabet = "QWERTYUIOPASDFGHJKLZXCVBNM".split("");
} // the ALPHABET in the order of the keys

var cells1 = []; // the cells in row one
var cells2 = []; // the cells in row two
var cells3 = []; // the cells in row three
var cells4 = []; // the cells in row four
var cells5 = []; // the cells in row five
var cells6 = []; // the cells in row six
var keys = []; // the keys array
var rawInputText = []; // raw input text from typing
var letters1 = []; // letters from the cells in row one
var letters2 = []; // letters from the cells in row two
var letters3 = []; // letters from the cells in row three
var letters4 = []; // letters from the cells in row four
var letters5 = []; // letters from the cells in row five
var letters6 = []; // letters from the cells in row six
var streakArray = []; // gather wins and loses for streaks



// DETERMINE THE WORD ===========================================
function shuffleArray(array) {
    var counter = array.length;

    // While there are elements in the array
    while (counter > 0) {
        // Pick a random index
        var ind = Math.floor(Math.random() * counter);
        // Decrease counter by 1
        counter--;
        // And swap the last element with it
        var temp = array[counter];
        array[counter] = array[ind];
        array[ind] = temp;
    }
} // shuffle function
shuffleArray(theWords); // shuffle "theWords" array
var theChosenWordID = 0; // the word number
var theChosenWord = theWords[theChosenWordID]; // chosen word



// PICKLE CONFETTI ==============================================
var confetti = false; // if true, rain down the pickle confetti
var confettiAmount = 50; // number of confetti pickles
var confettiX = []; // x-position of each confetti pickle
var confettiY = []; // y-position of each confetti pickle
var confettiAngle = []; // angle of each confetti pickle
var confettiPiece = []; // specific confetti pickle number
var confettiXRate = []; // rate of change of x-position
var confettiYRate = []; // rate of change of y-position
var confettiAngleRate = []; // rate of change of angle

// push values onto the pickle confetti arrays
for (var i = 0; i < confettiAmount; i++) {
    confettiX.push(random(-100, 700)); // push onto array
    confettiY.push(random(-1300, -20)); // push onto array
    confettiAngle.push(random(0, 360)); // push onto array
    confettiPiece.push(round(random(0.5, 2.49))); // add to array
    confettiXRate.push(random(-2, 2)); // push onto array
    confettiYRate.push(random(8, 12)); // push onto array
    confettiAngleRate.push(random(-9, 9)); // push onto array
}



// SPARKLES =====================================================
var sparkleAmount = 30; // number of sparkles
var sparkleTime = 0; // time during the sparkling
var sparkleX = []; // x-position of each sparkle
var sparkleY = []; // y-position of each sparkle
var sparkleSize = []; // size of each sparkle
var sparkleAngle = []; // angle of each sparkle
var sparkleInterval = []; // time at which each will sparkle
var sparkleAction = []; // status of each individual sparkle
var sparklePeriod = []; // time during each individual sparkle
var sparkleAmplitude = []; // aplitude of the cosine equation

// push values onto the sparkle arrays
for (var i = 0; i < sparkleAmount; i++) {
    sparkleX.push(random(80, 520)); // push onto array
    sparkleY.push(random(185, 465)); // push onto array
    sparkleSize.push(0); // push onto array
    sparkleAngle.push(random(0, 360)); // push onto array
    sparkleInterval.push(round(random(0, 400))); // push on array
    sparkleAction.push(false); // push onto array
    sparklePeriod.push(0); // push onto array
    sparkleAmplitude.push(random(5, 12.5)); // push onto array
}



// RAIN =========================================================
var rainAmount = 50; // number of rain drops
var rainX = []; // x-position of each rain drop
var rainY = []; // y-position of each rain drop

// push values onto the rain arrays
for (var i = 0; i < rainAmount; i++) {
    rainX.push(random(77.5, 522.5)); // push onto array
    rainY.push(random(-500, 100)); // push onto array
}



// COLORS =======================================================
var backgroundColor = color(255, 255, 255); // white
var lightGrayColor = color(211, 214, 218); // light gray
var grayIconColor = color(178, 193, 212); // icon color
var grayColor = color(161, 165, 179); // gray
var yellowColor = color(255, 190, 38); // yellow
var greenColor = color(74, 156, 29); // green
var blackColor = color(0, 0, 0); // black
var whiteColor = color(255, 255, 255); // white
var perfectColor = greenColor; // color of perfect cells
var almostColor = yellowColor; // color of almost cells
var sparkleColor = color(255, 177, 0); // color of each sparkle
var trophyStarColor = color(255, 156, 57); // color of star



// SOUNDS =======================================================
/*var winSound = getSound("retro/coin"); // sound for win scene
winSound.audio.volume = 0.5; // volume of the win sound

var correctSound = getSound("rpg/hit-thud"); // correct sound
correctSound.audio.preservesPitch = false; // enable pitch change
correctSound.audio.playbackRate = 5.7; // rate of correct sound
correctSound.audio.volume = 0.4; // volume of correct sound

var anotherSound = getSound("retro/hit1"); // low quiet sound
anotherSound.audio.volume = 0.05; // volume of low quiet sound

var incorrectSound = getSound("rpg/hit-whack"); // wrong sound
incorrectSound.audio.preservesPitch = false; // enable change
incorrectSound.audio.playbackRate = 6; // rate of incorrect sound
incorrectSound.audio.volume = 0.25; // volume of incorrect sound
*/


// FONTS ========================================================
var LatoIsSupported; // true if "Lato" supported, false if not
var themeFont = createFont("Lato"); // theme Lato font
var boldFont = createFont("Lato Bold"); // bold Lato font



// OBJECT-ORIENTED CELL =========================================

// constructor function -----------
var Cell = function(x, y, width, height, color, letter) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.color = color;
    this.letter = letter;
};

// draw each cell tile ------------
Cell.prototype.draw = function() {
    if (this.color === "lightGray") {
        stroke(lightGrayColor); // light gray
        noFill(); // no filling
    } else if (this.color === "yellow") {
        stroke(almostColor); // yellow (color blind mode = blue)
        fill(almostColor); // yellow (color blind mode = blue)
    } else if (this.color === "green") {
        stroke(perfectColor); // green (color blind mode = red)
        fill(perfectColor); // green (color blind mode = red)
    } else if (this.color === "gray") {
        stroke(grayColor); // gray
        fill(grayColor); // gray
    }
    
    // draw each cell tile
    strokeWeight(3); // thickness of outlines of cell tiles
    rect(this.x+cellsX, this.y, this.width, this.height, 5);
    
    // letter on the cell tile
    textFont(boldFont, 32); // bold font at size 32
    if (this.color === "lightGray") {
        fill(blackColor); // black text
    } else if (this.color === "yellow") {
        fill(255, 255, 255); // white text
    } else if (this.color === "green") {
        fill(255, 255, 255); // white text
    } else if (this.color === "gray") {
        fill(255, 255, 255); // white text
    }
    text(this.letter, this.x+1, this.y+1); // the letter
};

// define each row of cell tiles --
for (var i = 0; i < 5; i++) {
    cells1.push(new Cell(i*56+186, 125, 49, 49, "lightGray", ""));
} // first row of cells
for (var i = 0; i < 5; i++) {
    cells2.push(new Cell(i*56+186, 181, 49, 49, "lightGray", ""));
} // second row of cells
for (var i = 0; i < 5; i++) {
    cells3.push(new Cell(i*56+186, 237, 49, 49, "lightGray", ""));
} // third row of cells
for (var i = 0; i < 5; i++) {
    cells4.push(new Cell(i*56+186, 293, 49, 49, "lightGray", ""));
} // fourth row of cells
for (var i = 0; i < 5; i++) {
    cells5.push(new Cell(i*56+186, 349, 49, 49, "lightGray", ""));
} // fifth row of cells
for (var i = 0; i < 5; i++) {
    cells6.push(new Cell(i*56+186, 405, 49, 49, "lightGray", ""));
} // sixth row of cells



// OBJECT-ORIENTED KEY ==========================================

// constructor function -----------
var Key = function(x, y, width, height, color, letter, icon) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.color = color;
    this.letter = letter;
    this.icon = icon;
};
// draw each key ------------------
Key.prototype.draw = function() {
    noStroke(); // no outlines
    
    // determine the color of each key
    if (this.color === "lightGray") {
        if (darkMode) {
            if (this.isMouseInside()) {
                if (mousePressed) {
                    fill(59, 64, 69);
                } else {
                    fill(68, 73, 79);
                }
                cursor("pointer");
            } else {
                fill(lightGrayColor);
            }
        } else {
            if (this.isMouseInside()) {
                if (mousePressed) {
                    fill(157, 164, 173);
                } else {
                    fill(191, 195, 201);
                }
                cursor("pointer");
            } else {
                fill(lightGrayColor);
            }
        }
    } else if (this.color === "green") {
        if (this.isMouseInside()) {
            cursor("pointer");
        }
        fill(perfectColor);
    } else if (this.color === "yellow") {
        if (this.isMouseInside()) {
            cursor("pointer");
        }
        fill(almostColor);
    } else if (this.color === "gray") {
        if (this.isMouseInside()) {
            cursor("pointer");
        }
        fill(grayColor);
    }
    
    // draw the key square
    rect(this.x, this.y, this.width, this.height, 5); // the key
    
    // draw the letter or icon on the key
    if (this.letter === "↩") {
        pushMatrix();
        translate(this.x, this.y);
        if (darkMode) {
            stroke(255, 255, 255);
        } else {
            stroke(0, 0, 0);
        }
        strokeWeight(1.8);
        noFill();
        bezier(-10, 0, 6, 0, 10, 2, 10, -7);
        beginShape();
        vertex(-5, -5);
        vertex(-10, 0);
        vertex(-5, 5);
        endShape();
        popMatrix();
    } else if (this.letter === "⌫") {
        pushMatrix();
        translate(this.x+0.9, this.y);
        if (darkMode) {
            stroke(255, 255, 255);
        } else {
            stroke(0, 0, 0);
        }
        strokeWeight(1.5);
        noFill();
        beginShape();
        vertex(-16, 0);
        vertex(-8, 8);
        vertex(12, 8);
        vertex(12, -8);
        vertex(-8, -8);
        vertex(-16, 0);
        endShape();
        line(-4, -4, 4, 4);
        line(4, -4, -4, 4);
        popMatrix();
    } else {
        if (this.color === "lightGray") {
            if (darkMode) {
                fill(255, 255, 255);
            } else {
                fill(0, 0, 0);
            }
        } else {
            fill(255, 255, 255);
        }
        textFont(themeFont, 24);
        text(this.letter, this.x, this.y);
    }
};

// is mouse inside? ---------------
Key.prototype.isMouseInside = function() {
    return mouseX > (this.x - this.width/2) &&
           mouseX < (this.x + this.width/2) &&
           mouseY > (this.y - this.height/2) &&
           mouseY < (this.y + this.height/2) &&
           mouseOverCanvas && play;
};

// handle mouse click -------------
Key.prototype.handleMouseClick = function() {
    if (this.isMouseInside()) {
        if (this.letter === "↩" && gameOver === false) {
            enter = true;
        } else if (this.letter === "⌫" && rawInputText.length > backspaceLimit && gameOver === false) {
            enter = false;
            rawInputText.pop();
        } else if (this.letter !== "↩" && this.letter !== "⌫" && rawInputText.length < letterLimit && gameOver === false) {
            enter = false;
            rawInputText.push(this.letter);
        }
    }
};

// define each key on keyboard ----
for (var i = 0; i < 10; i++) {
        var keyX = i * 55 + 50;
        var keyY = 480;
        var keyLetter = alphabet[i];
        keys.push(new Key(keyX, keyY, 50, 40, "lightGray", keyLetter));
} // first row of keys
for (var i = 0; i < 9; i++) {
        var keyX = i * 61.3 + 53;
        var keyY = 525;
        var keyLetter = alphabet[i+10];
        keys.push(new Key(keyX, keyY, 56, 40, "lightGray", keyLetter));
} // second row of keys
keys.push(new Key(63, 570, 77, 40, "lightGray", "↩")); // enter
for (var i = 0; i < 7; i++) {
        var keyX = i * 55 + 133;
        var keyY = 570;
        var keyLetter = alphabet[i+19];
        keys.push(new Key(keyX, keyY, 50, 40, "lightGray", keyLetter));
} // third row of keys
keys.push(new Key(532, 570, 77, 40, "lightGray", "⌫")); // ⌫



// FUNCTIONS ====================================================
function PickleLogo(x, y, size) {
    pushMatrix(); // beginning of the Pickle logo
    translate(x, y-1); // position of the Pickle logo
    scale(size); // size of the Pickle logo
    
    stroke(blackColor);
    strokeWeight(8);
    noFill();
    line(-72, -20, -72, 20);
    arc(-68, -8, 29, 25, 4.468043, 7.941248);
    line(-41, -5, -41, 20);
    point(-41, -17);
    arc(-17, 7.5, 25, 25, 1.047198, 5.235988);
    line(2, -20, 2, 20);
    line(3, 5, 17, 20);
    line(3, 10, 17, -5);
    arc(60, 7.5, 24, 25, 2.356194, 6.195919);
    arc(62, 7.5, 30, 25, 1.134464, 2.286381);
    line(51, 7, 72, 7);
    
    stroke(74, 156, 29);
    strokeWeight(15);
    arc(82, 4, 100, 100, 2.879793, 3.577925);
    strokeWeight(6);
    point(29, -16);
    point(26, 0);
    point(27, 12);
    point(39, -6);
    point(38, 8);
    stroke(0, 0, 0, 50);
    strokeWeight(2);
    arc(35, -13, 4, 4, 2.792527, 5.934119);
    arc(34, -1, 4, 4, 4.712389, 7.853982);
    arc(34, 15, 4, 4, 0.349066, 3.316126);
    
    popMatrix(); // end of the Pickle logo
} // Pickle logo
function thePickle(x, y, size, angle) {
    pushMatrix();
    translate(x, y);
    scale(size);
    rotate(angle);
    
    stroke(74, 156, 29);
    strokeWeight(15);
    noFill();
    arc(50, 4, 100, 100, 2.879793, 3.577925);
    strokeWeight(6);
    point(-3, -16);
    point(-6, 0);
    point(-5, 12);
    point(7, -6);
    point(6, 8);
    stroke(0, 0, 0, 50);
    strokeWeight(2);
    arc(3, -13, 4, 4, 2.792527, 5.934119);
    arc(2, -1, 4, 4, 4.712389, 7.853982);
    arc(2, 15, 4, 4, 0.349066, 3.316126);
    
    popMatrix();
} // the pickle icon
function trophyIcon(x, y, size, angle) {
    
    pushMatrix();
    translate(x, y);
    scale(size);
    rotate(angle);
    
    noStroke();
    fill(yellowColor);
    arc(0, 0, 67, 70, 0, PI);
    quad(-27, -62, -25, 2, 25, 2, 27, -62);
    arc(-22, -62, 40, 170, 0, PI);
    arc(22, -62, 40, 170, 0, PI);
    triangle(0, 40, -20, 60, 20, 60);
    
    stroke(yellowColor);
    strokeWeight(9);
    strokeCap(SQUARE);
    noFill();
    line(-65, -59, 65, -59);
    arc(-36, -63, 50, 120, 0, PI);
    arc(36, -63, 50, 120, 0, PI);
    strokeCap(ROUND);
    arc(-30, 37, 55, 50, -0.087266, 1.48353);
    arc(30, 37, 55, 50, 1.658063, 3.228859);
    line(-25, 62, 25, 62);
    
    for (var i = 0; i < 5; i++) {
        pushMatrix();
        translate(0, -18);
        rotate(0.017453*i*72);
        noStroke();
        fill(trophyStarColor);
        triangle(0, -21, -10, 0, 10, 0);
        popMatrix();
    }
    
    
    popMatrix();
} // trophy icon
function pickleWithTrophyImage() {
    
    if (darkMode) {
        for (var i = 0; i < 8; i++) {
            fill(255, 255, 255, 40-i*5);
            if (i === 0) {
                rect(100+i*50, 300, 50, 248);
            } else {
                rect(100+i*50, 326, 50, 300);
            }
        }
    }
    
    noStroke();
    fill(lightGrayColor);
    rect(175, 461, 200, 30, 15);
    rect(175, 451, 200, 30);
    rect(175, 430, 200, 40, 15);
    rect(125, 430, 100, 40);
    rect(250, 466, 200, 20);
    rect(250, 461, 200, 30, 15);
    
    trophyIcon(294, 299, 0.75, -12*0.017453);
    
    stroke(0, 0, 0);
    strokeWeight(5);
    noFill();
    beginShape();
    curveVertex(198, 222);
    curveVertex(194, 284);
    curveVertex(248, 283);
    curveVertex(313, 195);
    endShape();
    
    beginShape();
    curveVertex(186, 510);
    curveVertex(180, 363);
    curveVertex(196, 407);
    curveVertex(92, 488);
    endShape();
    
    
    noStroke();
    fill(0, 0, 0);
    ellipse(247, 284, 11, 11);
    ellipse(113, 258, 11, 11);
    arc(201, 410, 16, 12, 190, 360);
    arc(134, 410, 16, 12, 192, 360);
    
    
    thePickle(177, 293, -3.5, -10*0.017453);
    
    stroke(0, 0, 0);
    strokeWeight(5);
    noFill();
    beginShape();
    curveVertex(194, 197);
    curveVertex(153, 296);
    curveVertex(112, 256);
    curveVertex(150, 195);
    endShape();
    
    beginShape();
    curveVertex(172, 412);
    curveVertex(151, 363);
    curveVertex(139, 407);
    curveVertex(170, 482);
    endShape();
    
    
    noStroke();
    pushMatrix();
    translate(174, 249);
    rotate(-0.261799);
    fill(greenColor);
    ellipse(-10, 0, 25, 67);
    ellipse(10, 0, 25, 34);
    fill(0, 0, 0, 45);
    ellipse(-10, -3, 19, 32);
    ellipse(10, -3, 19, 32);
    fill(255, 255, 255);
    ellipse(-10, 0, 18, 25);
    ellipse(10, 0, 18, 25);
    fill(0, 0, 0);
    ellipse(-8, 0, 9, 9);
    ellipse(12, 0, 9, 9);
    arc(2, 19, 15, 23, 0, PI);
    popMatrix();
    
} // winning image
function sadPickleImage() {
    
    if (darkMode) {
        for (var i = 0; i < 8; i++) {
            fill(255, 255, 255, 40-i*5);
            if (i === 0) {
                rect(100+i*50, 300, 50, 248);
            } else {
                rect(100+i*50, 326, 50, 300);
            }
        }
    }
    
    noStroke();
    fill(lightGrayColor);
    rect(300, 461, 450, 30, 15);
    beginShape();
    vertex(75, 460);
    vertex(75, 420);
    vertex(100, 407);
    vertex(110, 405);
    vertex(200, 405);
    vertex(286, 390);
    vertex(316, 395);
    vertex(420, 438);
    vertex(480, 443);
    vertex(525, 435);
    vertex(525, 460);
    vertex(75, 460);
    endShape();
    
    stroke(0, 0, 0);
    strokeWeight(5);
    noFill();
    beginShape();
    curveVertex(252, 366);
    curveVertex(194, 293);
    curveVertex(213, 341);
    curveVertex(246, 477);
    endShape();
    
    beginShape();
    curveVertex(186, 510);
    curveVertex(180, 363);
    curveVertex(196, 407);
    curveVertex(92, 488);
    endShape();
    
    
    noStroke();
    fill(0, 0, 0);
    ellipse(212, 344, 11, 11);
    ellipse(129, 347, 11, 11);
    arc(201, 410, 16, 12, 3.316126, 2*PI);
    arc(134, 410, 16, 12, 3.351032, 2*PI);
    
    
    pushMatrix();
    translate(172, 293);
    scale(3.5);
    rotate(0.10472);
    
    stroke(74, 156, 29);
    strokeWeight(15);
    noFill();
    arc(50, 4, 100, 100, 2.879793, 3.577925);
    strokeWeight(6);
    point(-3, -15);
    point(-6, 0);
    point(-5, 12);
    point(8, -10);
    point(6, 8);
    stroke(0, 0, 0, 50);
    strokeWeight(2);
    arc(-1, 3, 4, 4, 1.5708, 4.71239);
    arc(2, 15, 4, 4, -0.349066, 2.61799);
    
    popMatrix();
    
    stroke(0, 0, 0);
    strokeWeight(5);
    noFill();
    beginShape();
    curveVertex(183, 340);
    curveVertex(150, 288);
    curveVertex(131, 345);
    curveVertex(150, 406);
    endShape();
    
    beginShape();
    curveVertex(172, 412);
    curveVertex(151, 363);
    curveVertex(139, 407);
    curveVertex(170, 482);
    endShape();
    
    
    noStroke();
    pushMatrix();
    translate(192, 251);
    rotate(0.349066);
    fill(greenColor);
    ellipse(-10, 0, 25, 67);
    ellipse(10, 0, 25, 34);
    fill(0, 0, 0, 45);
    ellipse(-10, -2, 19, 30);
    ellipse(10, -2, 19, 30);
    fill(255, 255, 255);
    arc(-10, 0, 18, 25, 0, PI);
    arc(10, 0, 18, 25, 0, PI);
    arc(-10, 0, 18, 20, PI, 2*PI);
    arc(10, 0, 18, 20, PI, 2*PI);
    fill(0, 0, 0);
    ellipse(-8, 4, 9, 9);
    ellipse(12.5, 5, 9, 9);
    stroke(0, 0, 0);
    strokeWeight(5);
    noFill();
    arc(2, 25, 15, 9, 3.839724, 5.759587);
    popMatrix();
    
} // losing image
function hoverLink(x, y, width) {
    
    noStroke();
    fill(greenColor);
    arc(x-width/2, y+34, 11, 12, 3.159046, 4.712389);
    rect(x, y+31, width, 6);
    arc(x+width/2, y+34, 11, 12, 4.712389, 2*PI);
    
} // green hover underline
function restartIcon(x, y, size, angle) {
    pushMatrix();
    translate(x, y);
    scale(size);
    rotate(angle);
    
    stroke(grayIconColor);
    strokeWeight(4.5);
    strokeCap(SQUARE);
    noFill();
    arc(0, 0, 25, 25, 3.839724, 6.370452);
    arc(0, 0, 25, 25, 0.698132, 3.228859);
    
    noStroke();
    fill(grayIconColor);
    triangle(-13, -4, -13, -14, -3, -4);
    triangle(13, 4, 13, 14, 3, 4);
    
    popMatrix();
    
    strokeCap(ROUND); // reset strokeCap to default
} // restart
function howToPlayIcon(x, y, size) {
    pushMatrix();
    translate(x, y);
    scale(size);
    
    noStroke();
    fill(grayIconColor);
    ellipse(0, 0, 30, 30);
    fill(whiteColor);
    textFont(boldFont, 25);
    text("?", 0.5, 0);
    text("?", 0, 0);
    
    popMatrix();
} // how icon
function statisticsIcon(x, y, size) {
    pushMatrix();
    translate(x, y);
    scale(size);
    
    noStroke();
    fill(grayIconColor);
    rect(-10, 5, 7, 18);
    rect(0, 2.5, 7, 23);
    rect(10, 0, 7, 28);
    
    popMatrix();
} // stats icon
function settingsIcon(x, y, size) {
    pushMatrix();
    translate(x, y);
    scale(size);
    
    stroke(grayIconColor);
    strokeWeight(8);
    noFill();
    ellipse(0, 0, 16, 16);
    
    noStroke();
    fill(grayIconColor);
    for (var i = 0; i < 6; i++) {
        pushMatrix();
        rotate(i*1.047198);
        rect(0, -12, 9, 7, 3);
        popMatrix();
    }
    
    popMatrix();
} // settings icon
function exitIcon(x, y, size, color) {
    
    pushMatrix();
    translate(x, y);
    scale(size);
    
    noStroke();
    fill(color);
    beginShape();
    vertex(0, -2);
    vertex(-8, -10);
    vertex(-10, -8);
    vertex(-2, 0);
    vertex(-10, 8);
    vertex(-8, 10);
    vertex(0, 2);
    vertex(8, 10);
    vertex(10, 8);
    vertex(2, 0);
    vertex(10, -8);
    vertex(8, -10);
    vertex(0, -2);
    endShape();
    
    popMatrix();
    
} // exit icon



// OBJECT-ORIENTED BUTTON =======================================

// constructor function -----------
var Button = function(config) {
    this.x = config.x || 300;
    this.y = config.y || 300;
    this.width = config.width || 100;
    this.height = config.height || 50;
    this.label = config.label || "Nice Button";
    this.textSize = config.textSize || 15;
    this.outline = config.outline || false;
    this.onClick = config.onClick || function() {};
};

// draw each button ---------------
Button.prototype.draw = function() {
    
    if (this.outline) {
        
        stroke(greenColor); // green
        strokeWeight(3); // thickness of lines
        noFill(); // no filling
        
        if (this.isMouseInside()) {
            if (mousePressed) {
                fill(blackColor, 25);
                rect(this.x, this.y, this.width-2, this.height-2, 7);
                this.textSize = 15;
            } else {
                rect(this.x, this.y, this.width, this.height, 7);
                this.textSize = 15.5;
            }
        } else {
            rect(this.x, this.y, this.width-2, this.height-2, 7);
            this.textSize = 15;
        }
        
        textFont(boldFont, this.textSize); // font and size
        fill(greenColor); // green
        text(this.label, this.x, this.y); // text
        
    } else {
        noStroke(); // no outlines
        fill(greenColor); // green
        
        if (this.isMouseInside()) {
            if (mousePressed) {
                rect(this.x, this.y, this.width, this.height, 7);
                fill(blackColor, 25);
                rect(this.x, this.y, this.width, this.height, 7);
                this.textSize = 15;
            } else {
                rect(this.x, this.y, this.width+2, this.height+2, 7);
                this.textSize = 15.7;
            }
        } else {
            rect(this.x, this.y, this.width, this.height, 7);
            this.textSize = 15;
        }
        
        
        textFont(boldFont, this.textSize); // font and size
        fill(whiteColor); // white
        text(this.label, this.x, this.y); // text
    }
    
    if (this.isMouseInside()) {
        cursor("pointer"); // pointer cursor
    }
    
};

// is mouse inside? ---------------
Button.prototype.isMouseInside = function() {
    return mouseX > (this.x - this.width/2 - 1) &&
           mouseX < (this.x + this.width/2 + 1) &&
           mouseY > (this.y - this.height/2 - 1) &&
           mouseY < (this.y + this.height/2 + 1) &&
           mouseOverCanvas;
};

// handle mouse click -------------
Button.prototype.handleMouseClick = function() {
    if (this.isMouseInside()) {
        this.onClick();
    }
};

// define the buttons -------------
var ViewStatisticsButton = new Button({
    x: 420,
    y: 410,
    width: 140,
    height: 40,
    label: "View Statistics",
    outline: true,
    onClick: function() {
        statisticsStatus = true;
        howToPlayStatus = false;
        settingsStatus = false;
        winStatus = false;
        loseStatus = false;
        play = false;
    }
});
var NewGameButton = new Button({
    x: 420,
    y: 360,
    width: 140,
    height: 40,
    label: "New Game",
    onClick: function() {
        
        howToPlayStatus = false;
        statisticsStatus = false;
        settingsStatus = false;
        winStatus = false;
        loseStatus = false;
        play = true;
        gameOverTime = 0;
        enter = false;
        
        if (theChosenWordID < 120) {
            theChosenWordID++;
            theChosenWord = theWords[theChosenWordID];
        } else {
            shuffleArray(theWords);
            theChosenWordID = 0;
            theChosenWord = theWords[theChosenWordID];
        }
        
        
        rawInputText = [];
        for (var i = 0; i < 28; i++) {
            keys[i].color = "lightGray";
        }
        
        cells1 = [];
        cells2 = [];
        cells3 = [];
        cells4 = [];
        cells5 = [];
        cells6 = [];
        
        letterLimit = 5;
        backspaceLimit = 0;
        currentTry = 0;
        gameOver = false;
        win = false;
        lose = false;
        determineCellColors = false;
        
        for (var i = 0; i < 5; i++) {
            cells1.push(new Cell(i*56+186, 125, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells2.push(new Cell(i*56+186, 181, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells3.push(new Cell(i*56+186, 237, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells4.push(new Cell(i*56+186, 293, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells5.push(new Cell(i*56+186, 349, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells6.push(new Cell(i*56+186, 405, 49, 49, "lightGray", ""));
        }
        
        confetti = false;
        confettiAmount = 50; // number of confetti pickles
        confettiX = []; // x-position of each confetti pickle
        confettiY = []; // y-position of each confetti pickle
        confettiAngle = []; // angle of each confetti pickle
        confettiPiece = []; // specific confetti pickle number
        confettiXRate = []; // rate of change of x-position
        confettiYRate = []; // rate of change of y-position
        confettiAngleRate = []; // rate of change of angle
        
        // push values onto the pickle confetti arrays
        for (var i = 0; i < confettiAmount; i++) {
            confettiX.push(random(-100, 700)); // push onto array
            confettiY.push(random(-1300, -20)); // push onto array
            confettiAngle.push(random(0, 360)); // push onto array
            confettiPiece.push(round(random(0.5, 2.49))); // add to array
            confettiXRate.push(random(-2, 2)); // push onto array
            confettiYRate.push(random(8, 12)); // push onto array
            confettiAngleRate.push(random(-9, 9)); // push onto array
        }
        
    }
});



// OBJECT-ORIENTED ICON BUTTON ==================================

// constructor function -----------
var IconButton = function(config) {
    this.x = config.x || 300;
    this.y = config.y || 35;
    this.size = config.size || 1;
    this.width = config.width || 28;
    this.color = config.color || grayIconColor;
    this.icon = config.icon || PickleLogo();
    this.underline = config.underline || false;
    this.currentpage = config.currentPage || false;
    this.onClick = config.onClick || function() {};
    this.active = config.active || true;
    this.rotate = config.rotate || false;
    this.angle = config.angle || 0;
    this.spinTime = config.spinTime || 0;
};

// draw each icon button ----------
IconButton.prototype.draw = function() {
    
    if (this.rotate) {
        this.icon(this.x, this.y, this.size, this.angle);
    } else if (this.underline === false) {
        this.icon(this.x, this.y, this.size, this.color);
    } else {
        this.icon(this.x, this.y, this.size);
    }
    
    
    if (this.isMouseInside()) {
        if (this.underline) {
            hoverLink(this.x, this.y, this.width);
        } else {
            this.color = color(0, 0, 0, 90);
        }
        cursor("pointer");
    } else {
        if (this.underline === false) {
            this.color = color(0, 0, 0, 40);
        }
    }
    
    if (this.currentPage && this.underline) {
        hoverLink(this.x, this.y, this.width);
    }
    
    if (this.rotate) {
        this.spinTime++;
        if (this.angle > -2*PI+0.1) {
            this.angle = (PI * cos(this.spinTime*6*0.017453) - PI);
        } else {
            this.spinTime = 0;
            this.angle = 0;
            this.rotate = false;
        }
    }
    
};

// is mouse inside? ---------------
IconButton.prototype.isMouseInside = function() {
    if (this.underline) {
        return mouseX > (this.x - this.width/2 - 7) &&
               mouseX < (this.x + this.width/2 + 7) &&
               mouseY > (this.y - 36) &&
               mouseY < (this.y + 36) &&
               mouseOverCanvas && this.active;
    } else {
        return mouseX > (this.x - 25) &&
               mouseX < (this.x + 25) &&
               mouseY > (this.y - 25) &&
               mouseY < (this.y + 25) &&
               mouseOverCanvas && this.active;
    }
};

// handle mouse click -------------
IconButton.prototype.handleMouseClick = function() {
    if (this.isMouseInside()) {
        this.onClick();
    }
};

// define the icon buttons --------
var PickleLogoButton = new IconButton({
    size: 0.80,
    width: 130,
    icon: PickleLogo,
    underline: true,
    onClick: function() {
        howToPlayStatus = false;
        statisticsStatus = false;
        settingsStatus = false;
        winStatus = false;
        loseStatus = false;
        play = true;
    }
});
var RestartButton = new IconButton({
    x: 30,
    icon: restartIcon,
    underline: true,
    onClick: function() {
        
        moveCells = true;
        
        this.rotate = true;
        
        howToPlayStatus = false;
        statisticsStatus = false;
        settingsStatus = false;
        winStatus = false;
        loseStatus = false;
        play = true;
        gameOverTime = 0;
        enter = false;
        
        if (theChosenWordID < 120) {
            theChosenWordID++;
            theChosenWord = theWords[theChosenWordID];
        } else {
            shuffleArray(theWords);
            theChosenWordID = 0;
            theChosenWord = theWords[theChosenWordID];
        }
        
        
        rawInputText = [];
        for (var i = 0; i < 28; i++) {
            keys[i].color = "lightGray";
        }
        
        cells1 = [];
        cells2 = [];
        cells3 = [];
        cells4 = [];
        cells5 = [];
        cells6 = [];
        
        letterLimit = 5;
        backspaceLimit = 0;
        currentTry = 0;
        gameOver = false;
        win = false;
        lose = false;
        determineCellColors = false;
        
        for (var i = 0; i < 5; i++) {
            cells1.push(new Cell(i*56+186, 125, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells2.push(new Cell(i*56+186, 181, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells3.push(new Cell(i*56+186, 237, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells4.push(new Cell(i*56+186, 293, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells5.push(new Cell(i*56+186, 349, 49, 49, "lightGray", ""));
        }
        for (var i = 0; i < 5; i++) {
            cells6.push(new Cell(i*56+186, 405, 49, 49, "lightGray", ""));
        }
        
        confetti = false;
        confettiAmount = 50; // number of confetti pickles
        confettiX = []; // x-position of each confetti pickle
        confettiY = []; // y-position of each confetti pickle
        confettiAngle = []; // angle of each confetti pickle
        confettiPiece = []; // specific confetti pickle number
        confettiXRate = []; // rate of change of x-position
        confettiYRate = []; // rate of change of y-position
        confettiAngleRate = []; // rate of change of angle
        
        // push values onto the pickle confetti arrays
        for (var i = 0; i < confettiAmount; i++) {
            confettiX.push(random(-100, 700)); // push onto array
            confettiY.push(random(-1300, -20)); // push onto array
            confettiAngle.push(random(0, 360)); // push onto array
            confettiPiece.push(round(random(0.5, 2.49))); // add to array
            confettiXRate.push(random(-2, 2)); // push onto array
            confettiYRate.push(random(8, 12)); // push onto array
            confettiAngleRate.push(random(-9, 9)); // push onto array
        }
        
    }
});
var HowToPlayButton = new IconButton({
    x: 488,
    icon: howToPlayIcon,
    underline: true,
    onClick: function() {
        howToPlayStatus = true;
        statisticsStatus = false;
        settingsStatus = false;
        winStatus = false;
        loseStatus = false;
        play = false;
    }
});
var StatisticsButton = new IconButton({
    x: 529,
    icon: statisticsIcon,
    underline: true,
    onClick: function() {
        statisticsStatus = true;
        howToPlayStatus = false;
        settingsStatus = false;
        winStatus = false;
        loseStatus = false;
        play = false;
    }
});
var SettingsButton = new IconButton({
    x: 570,
    icon: settingsIcon,
    underline: true,
    onClick: function() {
        settingsStatus = true;
        howToPlayStatus = false;
        statisticsStatus = false;
        winStatus = false;
        loseStatus = false;
        play = false;
    }
});
var ExitButton = new IconButton({
    x: 500,
    y: 150,
    icon: exitIcon,
    onClick: function() {
        play = true;
        howToPlayStatus = false;
        statisticsStatus = false;
        settingsStatus = false;
        winStatus = false;
        loseStatus = false;
    }
});



// OBJECT-ORIENTED TOGGLE SWITCH ================================

// constructor function -----------
var ToggleSwitch = function(config) {
    this.x = config.x || 465;
    this.y = config.y || 300;
    this.color = config.color || lightGrayColor;
    this.state = config.state || "OFF";
    this.toggleX = config.toggleX || this.x-15;
    this.onClick = config.onClick || function() {};
};

// draw each toggle switch --------
ToggleSwitch.prototype.draw = function() {
    
    noStroke();
    fill(this.color);
    rect(this.x, this.y, 30, 35);
    ellipse(this.x-15, this.y+0.5, 35, 35);
    ellipse(this.x+15, this.y+0.5, 35, 35);
    
    fill(0, 0, 0, 150);
    textFont(boldFont, 11);
    text(this.state, this.x+(this.x-this.toggleX)*0.9, this.y);
    
    fill(255, 255, 255);
    ellipse(this.toggleX, this.y+0.5, 27, 27);
    
    if (this.isMouseInside()) {
        cursor("pointer");
    }
    
    if (this.state === "OFF") {
        if (this.toggleX > this.x-15) {
            this.toggleX-=5;
        }
        this.color = lightGrayColor;
    } else if (this.state === "ON") {
        if (this.toggleX < this.x+15) {
            this.toggleX+=5;
        }
        this.color = greenColor;
    }
};

// is mouse inside? ---------------
ToggleSwitch.prototype.isMouseInside = function() {
    return mouseX > (this.x - 34) &&
           mouseX < (this.x + 34) &&
           mouseY > (this.y - 17.5) &&
           mouseY < (this.y + 17.5) &&
           mouseOverCanvas;
};

// handle mouse click -------------
ToggleSwitch.prototype.handleMouseClick = function() {
    if (this.isMouseInside()) {
        if (this.state === "OFF") {
            this.state = "ON";
        } else if (this.state === "ON") {
            this.state = "OFF";
        }
        this.onClick();
    }
};

// define the toggle switches -----
var DarkModeSwitch = new ToggleSwitch({
    y: 220,
    onClick: function() {
        if (this.state === "OFF") {
            blackColor = color(0, 0, 0);
            whiteColor = color(255, 255, 255);
            lightGrayColor = color(211, 214, 218);
            grayIconColor = color(178, 193, 212);
            grayColor = color(161, 165, 179);
            darkMode = false;
        } else if (this.state === "ON") {
            blackColor = color(255, 255, 255);
            whiteColor = color(38, 39, 46);
            lightGrayColor = color(87, 94, 102);
            grayIconColor = color(87, 94, 102);
            grayColor = color(66, 68, 74);
            darkMode = true;
        }
    }
});
var ColorBlindModeSwitch = new ToggleSwitch({
    y: 290,
    onClick: function() {
        if (this.state === "OFF") {
            perfectColor = greenColor;
            almostColor = yellowColor;
        } else if (this.state === "ON") {
            perfectColor = color(245, 121, 58);
            almostColor = color(133, 192, 249);
        }
    }
});
var SoundOnOffSwitch = new ToggleSwitch({
    y: 360,
    state: "ON",
    toggleX: this.x+15,
    color: greenColor,
    onClick: function() {
        if (this.state === "OFF") {
            soundStatus = false;
        } else if (this.state === "ON") {
            soundStatus = true;
        }
    }
});



// MORE FUNCTIONS ===============================================
function determineConfettiPiece(pieceNumber) {
    
    // determine shape of each confetti pickle
    if (pieceNumber === 1) {
        stroke(74, 156, 29);
        strokeWeight(15);
        noFill();
        arc(50, 4, 100, 100, 2.879793, 3.577925);
        strokeWeight(6);
        point(-3, -16);
        point(-6, 0);
        point(-5, 12);
        point(7, -6);
        point(6, 8);
        stroke(0, 0, 0, 50);
        strokeWeight(2);
        arc(3, -13, 4, 4, 2.792527, 5.934119);
        arc(2, -1, 4, 4, 4.712389, 7.853982);
        arc(2, 15, 4, 4, 0.349066, 3.316126);
    } else if (pieceNumber === 2) {
        stroke(74, 156, 29);
        strokeWeight(15);
        noFill();
        arc(-50, 4, 100, 100, -0.436332, 0.261799);
        strokeWeight(6);
        point(3, -16);
        point(6, 0);
        point(5, 12);
        point(-7, -6);
        point(-6, 8);
        stroke(0, 0, 0, 50);
        strokeWeight(2);
        arc(-3, -13, 4, 4, 3.490659, 6.632251);
        arc(-2, -1, 4, 4, 1.570796, 4.712389);
        arc(-2, 15, 4, 4, -0.174533, 2.792527);
    }
    
} // which one
function drawPickleConfetti() {
    // draw each individual piece of confetti
    for (var i = 0; i < confettiAmount; i++) {
        if (confettiY[i] < 625) {
            pushMatrix(); // beginning of each confetti piece
            translate(confettiX[i], confettiY[i]); // position
            scale(2);
            rotate(confettiAngle[i]*0.017453); // rotating
            determineConfettiPiece(confettiPiece[i]);
            popMatrix(); // end of each confetti piece
            confettiX[i] += confettiXRate[i]; // left or right
            confettiY[i] += confettiYRate[i]; // falls downward
            confettiAngle[i] += confettiAngleRate[i]; // rotates
        }
    }
} // draw the pickle confetti
function drawSparkles() {
    
    if (sparkleTime < 400) {
        sparkleTime++;
    } else {
        sparkleTime = 0;
    }
    //println(sparkleTime);
    for (var i = 0; i < sparkleAmount; i++) {
        if (sparkleInterval[i] === sparkleTime) {
            sparkleAction[i] = true;
        }
        if (sparkleAction[i]) {
            sparklePeriod[i]++;
            sparkleSize[i] = sparkleAmplitude[i]*cos(5*PI/180*sparklePeriod[i])-sparkleAmplitude[i];
            if (sparkleSize[i] === 0) {
                sparkleAction[i] = false;
            }
        }
        pushMatrix();
        translate(sparkleX[i], sparkleY[i]);
        rotate(sparkleAngle[i]);
        noStroke();
        fill(sparkleColor);
        rect(0, 0, 0.1, 0.1, sparkleSize[i]);
        popMatrix();
    }
    
} // draw the sparkles
function drawRain() {
    for (var i = 0; i < rainAmount; i++) {
        if (rainY[i] > 179 && rainY[i] < 475) {
            noStroke();
            fill(124, 179, 196, 100);
            ellipse(rainX[i], rainY[i], 4, 15);
        }
        rainY[i]+=10;
        if (rainY[i] > 500) {
            rainY[i] = 0;
        }
    }
} // draw the rain
function findDuplicates(string) {
    var result = [];
    for (var i = 0; i < string.length; i++) {
        if (string.lastIndexOf(string[i]) > i && result.indexOf(string[i]) === -1) {
            result.push(string[i]);
        }
    }
    return result;
} // search for duplicates
function countLetter(string, letter) {
    var count = 0;
        for (var i = 0; i < string.length; i++) {
            if (string[i] === letter) {
                count++;
            }
        }
    return count;
} // 
function calculateMaxStreak(array) {
    var count = 0; // start with count of 0
    var result = 0;
    
    for (var i = 0; i < array.length; i++) {
        if (array[i] === 1) {
            count++; // increment count if element is 1
            result = Math.max(result, count); // update result with maximum count
        } else {
            count = 0; // reset count to 0 if element is 0
        }
    }
    
    return result;
} // calculate max streak
function calculateCurrentStreak(arr) {
    var currentStreak = 0;
    
    // Loop through the array, starting from the end
    for (var i = arr.length - 1; i >= 0; i--) {
        // If we encounter a 0, stop counting the streak
        if (arr[i] === 0) {
            break;
        }
        
        // If we encounter a 1, increment the streak counter
        currentStreak++;
    }
    
    return currentStreak;
} // get current streak
function alert(message) {
    
    fill(blackColor);
    textFont(boldFont, 20);
    text(message, 300, 85);
    
} // alert box template
function popupInfoBox(title) {
    
    noStroke();
    fill(0, 0, 0, darkenOpacity);
    rect(300, 300, 700, 700);
    
    for (var i = 0; i < 7; i++) {
        fill(0, 0, 0, 7);
        rect(300, 300, 448+i*2, 348+i*2, 18);
    }
    
    fill(whiteColor);
    rect(300, 302, 450, 348, 15);
    
    noStroke();
    fill(greenColor);
    rect(300, 150, 450, 50, 15);
    rect(300, 156, 450, 40, 0);
    
    textFont(boldFont, 30);
    for (var i = 0; i < 4; i++) {
        fill(0, 0, 0, 10);
        text(title, 300+i, 150+i/2);
    }
    fill(255, 255, 255);
    text(title, 300, 150);
    
    ExitButton.draw();
    
} // pop-up template
function introduction() {
    
    fill(255, 255, 255, introOpacity);
    rect(300, 300, 700, 700);
    
    PickleLogo(introX, 300, 3);
    
    introTime++;
    
    if (introTime < 180) {
        PickleLogoButton.active = false;
        RestartButton.active = false;
        HowToPlayButton.active = false;
        StatisticsButton.active = false;
        SettingsButton.active = false;
        introXRate = sq(introTime-73)/220+0.25;
        if (introTime >= 110) {
            if (cellsX > 0) {
                cellsX-=10;
            }
        }
        if (introTime >= 150) {
            if (introOpacity > 0) {
                introOpacity-=10;
            } else {
                play = true;
                PickleLogoButton.active = true;
                RestartButton.active = true;
                HowToPlayButton.active = true;
                StatisticsButton.active = true;
                SettingsButton.active = true;
                intro = false;
            }
        }
    }
    
    introX += introXRate;
    
    stroke(greenColor);
    strokeWeight(25);
    noFill();
    beginShape();
    for (var i = introX-500; i < introX; i++) {
        var x = -i+600;
        var y = 40*sin(3*0.017453*i)+80;
        vertex(x, y);
    }
    endShape();
    
    beginShape();
    for (var i = introX-500; i < introX; i++) {
        var x = i;
        var y = 40*sin(3*0.017453*i)+520;
        vertex(x, y);
    }
    endShape();
    
    fill(250, 250, 250, introOpacity);
    textFont(themeFont, 75);
    text("                   ∎", 0, -5);
    
    if (introTime === 100) {
        if (get(200, 10) === whiteColor) {
            LatoIsSupported = true;
            //println("Lato Supported!");
        } else {
            LatoIsSupported = false;
            //println("Sans-serif");
        }
    }
    
} // introduction
function howToPlay() {
    
    popupInfoBox("How to Play");
    
    stroke(blackColor);
    strokeWeight(5.5);
    point(105, 220);
    point(105, 245);
    point(105, 405);
    point(105, 430);
    point(105, 455);
    
    textAlign(LEFT, CENTER);
    textFont(boldFont, 20);
    fill(blackColor);
    text("Guess the target word in 6 tries.", 100, 195);
    textFont(themeFont, 17);
    fill(blackColor, 130);
    text("Each guess must be a valid 5-letter word.", 120, 220);
    text("The color of the tiles will change to show how close", 120, 245);
    text("your guess was to the word.", 120, 265);
    textFont(boldFont, 20);
    fill(blackColor);
    text("Example guess:", 100, 290);
    
    
    if (LatoIsSupported) {
        textFont(themeFont, 17);
        fill(blackColor, 130);
        text("The letter      is in the word and in the correct spot.", 120, 405);
        text("     and      are in the word but in the wrong locations.", 120, 430);
        text("The letters       and      are not in the word in any spots.", 120, 455);
        textAlign(CENTER, CENTER);
        
        textFont(boldFont, 18);
        fill(perfectColor);
        text("A", 203, 405);
        fill(almostColor);
        text("N", 126, 430);
        text("O", 174, 430);
        fill(129, 133, 145);
        text("M", 212, 455);
        text("G", 261, 455);
    } else {
        textFont(themeFont, 16);
        fill(blackColor, 130);
        text("The letter     is in the word and in the correct spot.", 120, 405);
        text("    and     are in the word but in the wrong locations.", 120, 430);
        text("The letters      and     are not in the word in any spots.", 120, 455);
        textAlign(CENTER, CENTER);
        
        textFont(boldFont, 18);
        fill(perfectColor);
        text("A", 199, 405);
        fill(almostColor);
        text("N", 126, 430);
        text("O", 176, 430);
        fill(129, 133, 145);
        text("M", 208, 455);
        text("G", 261, 455);
    }
    
    
    pushMatrix();
    translate(300, 347);
    scale(1.46);
    noStroke();
    fill(grayColor);
    rect(-112, 0, 51, 51, 5);
    fill(perfectColor);
    rect(-56, 0, 51, 51, 5);
    fill(almostColor);
    rect(0, 0, 51, 51, 5);
    fill(grayColor);
    rect(56, 0, 51, 51, 5);
    fill(almostColor);
    rect(112, 0, 51, 51, 5);
    
    textFont(boldFont, 32);
    fill(255, 255, 255);
    text("M", -112, 0);
    text("A", -56, 0);
    text("N", 0, 0);
    text("G", 56, 0);
    text("O", 112, 0);
    popMatrix();
    
    
} // "how to play" pop-up page
function statistics() {
    
    popupInfoBox("Statistics");
    
    fill(lightGrayColor, 230);
    rect(150, 235, 90, 70, 10);
    rect(250, 235, 90, 70, 10);
    rect(350, 235, 90, 70, 10);
    rect(450, 235, 90, 70, 10);
    
    fill(blackColor);
    textFont(themeFont, 30);
    text(statsGamesPlayed, 150, 235);
    text(statsPercentWon, 250, 235);
    text(statsCurrentStreak, 350, 235);
    text(statsMaxStreak, 450, 235);
    
    fill(blackColor, 160);
    textFont(themeFont, 13);
    text("Games Played", 150, 282);
    text("Percent Won", 250, 282);
    text("Current Streak", 350, 282);
    text("Max Streak", 450, 282);
    
    textFont(boldFont, 20);
    fill(blackColor);
    text("Guess Distribution", 300, 315);
    
    stroke(lightGrayColor);
    strokeWeight(2);
    line(105, 317, 210, 317);
    line(390, 317, 495, 317);
    
    textFont(boldFont, 15);
    fill(blackColor);
    text("1", 110, 345);
    text("2", 110, 365);
    text("3", 110, 385);
    text("4", 110, 405);
    text("5", 110, 425);
    text("6", 110, 445);
    
    noStroke();
    fill(lightGrayColor, 230);
    for (var i = 0; i < 6; i++) {
        rect(307.5, 345+i*20, 377.5, 15, 30);
    }
    
    var maxValue = Math.max(statsTry1, statsTry2, statsTry3, statsTry4, statsTry5, statsTry6);
    
    if (statsGamesWon === 0) {
        statsBarWidth1 = 20;
        statsBarWidth2 = 20;
        statsBarWidth3 = 20;
        statsBarWidth4 = 20;
        statsBarWidth5 = 20;
        statsBarWidth6 = 20;
    } else {
        statsBarWidth1 = statsTry1/maxValue*357+20;
        statsBarWidth2 = statsTry2/maxValue*357+20;
        statsBarWidth3 = statsTry3/maxValue*357+20;
        statsBarWidth4 = statsTry4/maxValue*357+20;
        statsBarWidth5 = statsTry5/maxValue*357+20;
        statsBarWidth6 = statsTry6/maxValue*357+20;
    }
    
    
    fill(greenColor);
    rect(120+statsBarWidth1/2, 345, statsBarWidth1, 17, 30);
    rect(120+statsBarWidth2/2, 365, statsBarWidth2, 17, 30);
    rect(120+statsBarWidth3/2, 385, statsBarWidth3, 17, 30);
    rect(120+statsBarWidth4/2, 405, statsBarWidth4, 17, 30);
    rect(120+statsBarWidth5/2, 425, statsBarWidth5, 17, 30);
    rect(120+statsBarWidth6/2, 445, statsBarWidth6, 17, 30);
    
    //println(statsTry6);
    
    fill(whiteColor);
    textFont(boldFont, 14);
    textAlign(RIGHT, CENTER);
    text(statsTry1, 120+statsBarWidth1-6, 345);
    text(statsTry2, 120+statsBarWidth2-6, 365);
    text(statsTry3, 120+statsBarWidth3-6, 385);
    text(statsTry4, 120+statsBarWidth4-6, 405);
    text(statsTry5, 120+statsBarWidth5-6, 425);
    text(statsTry6, 120+statsBarWidth6-6, 445);
    textAlign(CENTER, CENTER);
    
    //rect();
    
} // "statistics" pop-up page
function settings() {
    
    popupInfoBox("Settings");
    
    textAlign(LEFT, CENTER);
    fill(blackColor);
    textFont(boldFont, 25);
    text("Dark Mode", 100, 210);
    text("Color Blind Mode", 100, 280);
    text("Sound", 100, 350);
    
    fill(blackColor, 130);
    textFont(themeFont, 15);
    text("Switch to a darker color scheme.", 100, 235);
    text("High contrast mode for color deficiency.", 100, 305);
    text("Toggle game sound on/off.", 100, 375);
    
    thePickle(115, 440, 1, -35);
    
    if (LatoIsSupported) {
        textSize(14);
        text("Pickle is based on the concept of                     , a game\ncreated by The New York Times Company.", 150, 435);
        textAlign(CENTER, CENTER);
        
        if (mouseX > 347 && mouseX < 406 && mouseY > 415 && mouseY < 435) {
            
            textFont(boldFont, 15);
            fill(greenColor);
            text("Wordle", 376.5, 424);
            
            stroke(greenColor);
            strokeWeight(1);
            line(351, 432, 401, 432);
            
            cursor("pointer");
        } else {
            
            textFont(boldFont, 15);
            fill(blackColor);
            text("Wordle", 376.5, 424);
            
            stroke(blackColor);
            strokeWeight(1);
            line(351, 432, 401, 432);
            
        }
    } else {
        textSize(14);
        text(/*"Pickle is based off of Wordle by the New York Times Company."*/ "Pickle is based on the concept of               , a game\ncreated by The New York Times Company.", 150, 435);
        textAlign(CENTER, CENTER);
        
        if (mouseX > 357 && mouseX < 415 && mouseY > 415 && mouseY < 435) {
            
            textFont(boldFont, 15);
            fill(greenColor);
            text("Wordle", 386.5, 424);
            
            stroke(greenColor);
            strokeWeight(1);
            line(361, 432, 411, 432);
            
            cursor("pointer");
        } else {
            
            textFont(boldFont, 15);
            fill(blackColor);
            text("Wordle", 386.5, 424);
            
            stroke(blackColor);
            strokeWeight(1);
            line(361, 432, 411, 432);
            
        }
    }
    
    for (var i = 0; i < 3; i++) {
        stroke(lightGrayColor);
        strokeWeight(2);
        line(100, 255+i*70, 500, 255+i*70);
    }
    
    DarkModeSwitch.draw();
    ColorBlindModeSwitch.draw();
    SoundOnOffSwitch.draw();
    
} // "settings" pop-up page
function winPopup() {
    
    popupInfoBox("Congratulations!");
    
    drawSparkles();
    
    pickleWithTrophyImage();
    
    textFont(boldFont, 35);
    fill(blackColor);
    text("You guessed it!", 360, 205);
    textFont(themeFont, 21);
    
    if (LatoIsSupported) {
        textFont(themeFont, 21);
        text("The word was\nindeed              ", 430, 280);
        textFont(boldFont, 21);
        textAlign(LEFT, CENTER);
        text(theChosenWord.toLowerCase() + ".", 439, 294);
        textAlign(CENTER, CENTER);
        NewGameButton.draw();
        ViewStatisticsButton.draw();
    } else {
        textFont(themeFont, 21);
        textAlign(LEFT, CENTER);
        text("The word was\nindeed", 358, 280);
        textFont(boldFont, 24);
        text(theChosenWord.toLowerCase() + ".", 428, 294);
        textAlign(CENTER, CENTER);
        NewGameButton.draw();
        ViewStatisticsButton.draw();
    }
    
    
} // "win" pop-up page
function losePopup() {
    
    popupInfoBox("Game Over");
    
    sadPickleImage();
    
    drawRain();
    
    textFont(boldFont, 28);
    fill(blackColor);
    text("That's not the word.", 375, 210);
    
    if (LatoIsSupported) {
        textFont(themeFont, 21);
        text("The word was\nactually              ", 420, 280);
        textFont(boldFont, 21);
        textAlign(LEFT, CENTER);
        text(theChosenWord.toLowerCase() + ".", 433, 294);
        textAlign(CENTER, CENTER);
    } else {
        textFont(themeFont, 21);
        textAlign(LEFT, CENTER);
        text("The word was\nactually", 351, 280);
        textFont(boldFont, 24);
        text(theChosenWord.toLowerCase() + ".", 428, 294);
        textAlign(CENTER, CENTER);
    }
    
    
    NewGameButton.draw();
    ViewStatisticsButton.draw();
    
} // "lose" pop-up page
function playScene() {
    
    // draw row one of cells
    for (var i = 0; i < cells1.length ; i++) {
        cells1[i].draw();
        cells1[i].letter = rawInputText[i];
    }
    
    // draw row two of cells
    for (var i = 0; i < cells2.length; i++) {
        cells2[i].draw();
        cells2[i].letter = rawInputText[i+5];
    }
    
    // draw row three of cells
    for (var i = 0; i < cells3.length; i++) {
        cells3[i].draw();
        cells3[i].letter = rawInputText[i+10];
    }
    
    // draw row four of cells
    for (var i = 0; i < cells4.length; i++) {
        cells4[i].draw();
        cells4[i].letter = rawInputText[i+15];
    }
    
    // draw row five of cells
    for (var i = 0; i < cells5.length; i++) {
        cells5[i].draw();
        cells5[i].letter = rawInputText[i+20];
    }
    
    // draw row six of cells
    for (var i = 0; i < cells6.length; i++) {
        cells6[i].draw();
        cells6[i].letter = rawInputText[i+25];
    }
    
    // draw the keyboard, one key at a time
    for (var i = 0; i < keyAmount; i++) {
        keys[i].draw();
    }
    
    // retrieve the letters from the cells
    for (var i = 0; i < 5; i++) {
        letters1[i] = cells1[i].letter;
        letters2[i] = cells2[i].letter;
        letters3[i] = cells3[i].letter;
        letters4[i] = cells4[i].letter;
        letters5[i] = cells5[i].letter;
        letters6[i] = cells6[i].letter;
    }
    
    // join the cell's letters to make a string
    var word1 = letters1.join("").toLowerCase(); // join letters
    var word2 = letters2.join("").toLowerCase(); // join letters
    var word3 = letters3.join("").toLowerCase(); // join letters
    var word4 = letters4.join("").toLowerCase(); // join letters
    var word5 = letters5.join("").toLowerCase(); // join letters
    var word6 = letters6.join("").toLowerCase(); // join letters
    
    
    // if game is over, do lots of stuff
    if (gameOver) {
        
        if (win) {
            alert("You won!"); // alert: "You won!"
            gameOverTime++; // time incrementing during game over
            if (gameOverTime === 90 && soundStatus) {
                //playSound(winSound); // play the winning sound
            }
            if (gameOverTime === 100) {
                statsGamesPlayed++; // number of games played++
                statsGamesWon++; // number of games won++
                streakArray.push(1); // add a "1" to streak array
                winStatus = true; // display the "win" pop-up
                
                // if won on certain try, increment variable by 1
                if (currentTry === 0) {
                    statsTry1++;
                } else if (currentTry === 1) {
                    statsTry2++;
                } else if (currentTry === 2) {
                    statsTry3++;
                } else if (currentTry === 3) {
                    statsTry4++;
                } else if (currentTry === 4) {
                    statsTry5++;
                } else if (currentTry === 5) {
                    statsTry6++;
                }
            }
            confetti = true; // display the pickle confetti
            play = false; // halt the game controls (no typing)
            
        } else if (lose) {
            alert("You lost"); // alert: "You lost"
            gameOverTime++; // time during game over period
            if (gameOverTime === 100) {
                statsGamesPlayed++; // number of games played++
                streakArray.push(0); // add a "0" to streak array
                loseStatus = true; // display the "lose" pop-up
            }
            play = false; // halt game controls (no typing)
        }
    }
    
    
    // if the enter key is typed, define the limits of typing
    // also control the warnings and sounds
    if (enter) {
        if (rawInputText.length === 5) {
            if (dictionary.includes(word1)) {
                currentTry = 0;
                letterLimit = 10;
                backspaceLimit = 5;
                determineCellColors = true;
                incorrectSoundTime = false;
            } else {
                alert("Not in word list");
                playIncorrectSound = true;
            }
        } else if (rawInputText.length === 10) {
            if (dictionary.includes(word2)) {
                currentTry = 1;
                letterLimit = 15;
                backspaceLimit = 10;
                determineCellColors = true;
                incorrectSoundTime = false;
            } else {
                alert("Not in word list");
                playIncorrectSound = true;
            }
        } else if (rawInputText.length === 15) {
            if (dictionary.includes(word3)) {
                currentTry = 2;
                letterLimit = 20;
                backspaceLimit = 15;
                determineCellColors = true;
                incorrectSoundTime = false;
            } else {
                alert("Not in word list");
                playIncorrectSound = true;
            }
        } else if (rawInputText.length === 20) {
            if (dictionary.includes(word4)) {
                currentTry = 3;
                letterLimit = 25;
                backspaceLimit = 20;
                determineCellColors = true;
                incorrectSoundTime = false;
            } else {
                alert("Not in word list");
                playIncorrectSound = true;
            }
        } else if (rawInputText.length === 25) {
            if (dictionary.includes(word5)) {
                currentTry = 4;
                letterLimit = 30;
                backspaceLimit = 25;
                determineCellColors = true;
                incorrectSoundTime = false;
            } else {
                alert("Not in word list");
                playIncorrectSound = true;
            }
        } else if (rawInputText.length === 30) {
            if (dictionary.includes(word6)) {
                currentTry = 5;
                letterLimit = 35;
                backspaceLimit = 30;
                determineCellColors = true;
                incorrectSoundTime = false;
            } else {
                alert("Not in word list");
                playIncorrectSound = true;
            }
        } else {
            alert("Not enough letters");
            playIncorrectSound = true;
        }
    } else {
        determineCellColors = false;
        incorrectSoundTime = false;
    }
    
    
    // if invalid word, make a click sound
    if (playIncorrectSound && soundStatus) {
        incorrectSoundTime++; // time increases
        if (incorrectSoundTime === 2) {
            //playSound(incorrectSound); // play incorrect sound
        }
    } else {
        incorrectSoundTime = 0; // return to initial time
    }
    
    
    // retrieve number of double letters in the target word
    var duplicates = findDuplicates(theChosenWord.toUpperCase());
    
    
    // if valid word is entered, determine cell colors of the row
    if (determineCellColors) {
        
        
        // increase the number of tiles revealed
        if (cellFlipTime < 5) {
            cellFlipTime+=0.25; // sweep over word
            if (cellFlipTime === 5 && soundStatus) {
                //playSound(correctSound); // play correct sound
                //playSound(anotherSound); // low base sound
            }
        }
        
        
        // loop through all five letters
        for (var i = 0; i < cellFlipTime; i++) {
            
            
            // specificWord is the string of the current row
            // specificRow is the current row array
            if (currentTry === 0) {
                specificWord = word1.toUpperCase();
                specificRow = cells1;
                specificRowLetter = cells1[i].letter;
            } else if (currentTry === 1) {
                specificWord = word2.toUpperCase();
                specificRow = cells2;
                specificRowLetter = cells2[i].letter;
            } else if (currentTry === 2) {
                specificWord = word3.toUpperCase();
                specificRow = cells3;
                specificRowLetter = cells3[i].letter;
            } else if (currentTry === 3) {
                specificWord = word4.toUpperCase();
                specificRow = cells4;
                specificRowLetter = cells4[i].letter;
            } else if (currentTry === 4) {
                specificWord = word5.toUpperCase();
                specificRow = cells5;
                specificRowLetter = cells5[i].letter;
            } else if (currentTry === 5) {
                specificWord = word6.toUpperCase();
                specificRow = cells6;
                specificRowLetter = cells6[i].letter;
            }
            
            
            // put all six rows of words in an array
            var allCellsColor = [cells1, cells2, cells3, cells4, cells5, cells6];
            
            
            // make the chosen word capitalized
            var capitalizedChosenWord = theChosenWord.toUpperCase();
            
            
            // loop through the target word's colors
            for (var k = 0; k < 5; k++) {
                
            
            // if the letter is in the right spot, make it green
            if (specificRowLetter === capitalizedChosenWord[i]) {
                allCellsColor[currentTry][i].color = "green";
            }
            
            
            // determine the color of the cells
            if (cellFlipTime === 0.25) {        // 1st cell
                if (specificRow[0].letter === capitalizedChosenWord[k] && specificRow[0].color !== "green") {
                    if (
    (specificRow[0].letter === specificRow[1].letter && specificRow[1].letter === capitalizedChosenWord[1]) ||
    (specificRow[0].letter === specificRow[2].letter && specificRow[2].letter === capitalizedChosenWord[2]) ||
    (specificRow[0].letter === specificRow[3].letter && specificRow[3].letter === capitalizedChosenWord[3]) ||
    (specificRow[0].letter === specificRow[4].letter && specificRow[4].letter === capitalizedChosenWord[4])) {
                        specificRow[0].color = "gray"; // gray
                    } else {
                        specificRow[0].color = "yellow";// yellow
                    }
                }
            } else if (cellFlipTime === 1) {        // 2nd cell
                if (specificRow[1].letter === capitalizedChosenWord[k] && specificRow[1].color !== "green" && specificRow[1].letter !== specificRow[0].letter) {
                    if (
    (specificRow[1].letter === specificRow[2].letter && specificRow[2].letter === capitalizedChosenWord[2]) ||
    (specificRow[1].letter === specificRow[3].letter && specificRow[3].letter === capitalizedChosenWord[3]) ||
    (specificRow[1].letter === specificRow[4].letter && specificRow[4].letter === capitalizedChosenWord[4])) {
                        specificRow[1].color = "gray"; // gray
                    } else {
                        specificRow[1].color = "yellow";// yellow
                    }
                }
            } else if (cellFlipTime === 2) {        // 3rd cell
                if (specificRow[2].letter === capitalizedChosenWord[k] && specificRow[2].color !== "green" && specificRow[2].letter !== specificRow[1].letter && specificRow[2].letter !== specificRow[0].letter) {
                    if (
    (specificRow[2].letter === specificRow[3].letter && specificRow[3].letter === capitalizedChosenWord[3]) ||
    (specificRow[2].letter === specificRow[4].letter && specificRow[4].letter === capitalizedChosenWord[4])) {
                        specificRow[2].color = "gray"; // gray
                    } else {
                        specificRow[2].color = "yellow";// yellow
                    }
                }
            } else if (cellFlipTime === 3) {        // 4th cell
                if (specificRow[3].letter === capitalizedChosenWord[k] && specificRow[3].color !== "green" && specificRow[3].letter !== specificRow[2].letter && specificRow[3].letter !== specificRow[1].letter && specificRow[3].letter !== specificRow[0].letter) {
                    if (
    (specificRow[3].letter === specificRow[4].letter && specificRow[4].letter === capitalizedChosenWord[4])) {
                        specificRow[3].color = "gray"; // gray
                    } else {
                        specificRow[3].color = "yellow";// yellow
                    }
                }
            } else if (cellFlipTime === 4) {        // 5th cell
                if (specificRow[4].letter === capitalizedChosenWord[k] && specificRow[4].color !== "green" && specificRow[4].letter !== specificRow[3].letter && specificRow[4].letter !== specificRow[2].letter && specificRow[4].letter !== specificRow[1].letter && specificRow[4].letter !== specificRow[0].letter) {
                    specificRow[4].color = "yellow"; // yellow
                }
            }
            
            
            
            /** THE MOST COMPLEX ALGORITHM I HAVE EVER MADE **/
            // if there is DOUBLE LETTERS in the target word,
            // follow this algorithm to color the cells
            if (duplicates.length > 0) {
                if (cellFlipTime === 0.25) {
                    if (specificRow[0].letter === capitalizedChosenWord[k] && specificRow[0].color !== "green" && (specificRow[0].letter === duplicates[0] || specificRow[0].letter === duplicates[1])) {
                        if ((countLetter(specificWord, duplicates[0]) === 3 || countLetter(specificWord, duplicates[1]) === 3) && ((capitalizedChosenWord[1] !==  capitalizedChosenWord[2] || capitalizedChosenWord[1] !== specificRow[1].letter || capitalizedChosenWord[2] !== specificRow[2].letter) &&
        (capitalizedChosenWord[1] !== capitalizedChosenWord[3] || capitalizedChosenWord[1] !== specificRow[1].letter || capitalizedChosenWord[3] !== specificRow[3].letter) &&
        (capitalizedChosenWord[1] !== capitalizedChosenWord[4] || capitalizedChosenWord[1] !== specificRow[1].letter || capitalizedChosenWord[4] !== specificRow[4].letter) &&
        (capitalizedChosenWord[2] !== capitalizedChosenWord[3] || capitalizedChosenWord[2] !== specificRow[2].letter || capitalizedChosenWord[3] !== specificRow[3].letter) &&
        (capitalizedChosenWord[2] !== capitalizedChosenWord[4] || capitalizedChosenWord[2] !== specificRow[2].letter || capitalizedChosenWord[4] !== specificRow[4].letter) &&
        (capitalizedChosenWord[3] !== capitalizedChosenWord[4] || capitalizedChosenWord[3] !== specificRow[3].letter || capitalizedChosenWord[4] !== specificRow[4].letter))) {
                            specificRow[0].color = "yellow";
                        } else if (specificRow[0].letter === duplicates[0] && (countLetter(specificWord, duplicates[0]) === 1 || countLetter(specificWord, duplicates[0]) === 2)) {
                            specificRow[0].color = "yellow";
                        } else if (specificRow[0].letter === duplicates[1] && (countLetter(specificWord, duplicates[1]) === 1 || countLetter(specificWord, duplicates[1]) === 2)) {
                            specificRow[0].color = "yellow";
                        }
                    }
                } else if (cellFlipTime === 1) {
                    if (specificRow[1].letter === capitalizedChosenWord[k] && specificRow[1].color !== "green" && (specificRow[1].letter === duplicates[0] || specificRow[1].letter === duplicates[1] )) {
                        if ((countLetter(specificWord, duplicates[0]) === 3 || countLetter(specificWord, duplicates[1]) === 3) && ((capitalizedChosenWord[0] !==  capitalizedChosenWord[2] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[2] !== specificRow[2].letter) &&
        (capitalizedChosenWord[0] !== capitalizedChosenWord[3] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[3] !== specificRow[3].letter) &&
        (capitalizedChosenWord[0] !== capitalizedChosenWord[4] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[4] !== specificRow[4].letter) &&
        (capitalizedChosenWord[2] !== capitalizedChosenWord[3] || capitalizedChosenWord[2] !== specificRow[2].letter || capitalizedChosenWord[3] !== specificRow[3].letter) &&
        (capitalizedChosenWord[2] !== capitalizedChosenWord[4] || capitalizedChosenWord[2] !== specificRow[2].letter || capitalizedChosenWord[4] !== specificRow[4].letter) &&
        (capitalizedChosenWord[3] !== capitalizedChosenWord[4] || capitalizedChosenWord[3] !== specificRow[3].letter || capitalizedChosenWord[4] !== specificRow[4].letter))) {
                            specificRow[1].color = "yellow";
                        } else if (specificRow[1].letter === duplicates[0] && (countLetter(specificWord, duplicates[0]) === 1 || countLetter(specificWord, duplicates[0]) === 2)) {
                            specificRow[1].color = "yellow";
                        } else if (specificRow[1].letter === duplicates[1] && (countLetter(specificWord, duplicates[1]) === 1 || countLetter(specificWord, duplicates[1]) === 2)) {
                            specificRow[1].color = "yellow";
                        }
                    }
                } else if (cellFlipTime === 2) {
                    if (specificRow[2].letter === capitalizedChosenWord[k] && specificRow[2].color !== "green" && (specificRow[2].letter === duplicates[0] || specificRow[2].letter === duplicates[1] )) {
                        if ((countLetter(specificWord, duplicates[0]) === 3 || countLetter(specificWord, duplicates[1]) === 3) && ((capitalizedChosenWord[0] !==  capitalizedChosenWord[1] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[1] !== specificRow[1].letter) &&
        (capitalizedChosenWord[0] !== capitalizedChosenWord[3] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[3] !== specificRow[3].letter) &&
        (capitalizedChosenWord[0] !== capitalizedChosenWord[4] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[4] !== specificRow[4].letter) &&
        (capitalizedChosenWord[1] !== capitalizedChosenWord[3] || capitalizedChosenWord[1] !== specificRow[1].letter || capitalizedChosenWord[3] !== specificRow[3].letter) &&
        (capitalizedChosenWord[1] !== capitalizedChosenWord[4] || capitalizedChosenWord[1] !== specificRow[1].letter || capitalizedChosenWord[4] !== specificRow[4].letter) &&
        (capitalizedChosenWord[3] !== capitalizedChosenWord[4] || capitalizedChosenWord[3] !== specificRow[3].letter || capitalizedChosenWord[4] !== specificRow[4].letter))) {
                            specificRow[2].color = "yellow";
                        } else if (specificRow[2].letter === duplicates[0] && (countLetter(specificWord, duplicates[0]) === 1 || countLetter(specificWord, duplicates[0]) === 2)) {
                            specificRow[2].color = "yellow";
                        } else if (specificRow[2].letter === duplicates[1] && (countLetter(specificWord, duplicates[1]) === 1 || countLetter(specificWord, duplicates[1]) === 2)) {
                            specificRow[2].color = "yellow";
                        }
                    }
                } else if (cellFlipTime === 3) {
                    if (specificRow[3].letter === capitalizedChosenWord[k] && specificRow[3].color !== "green" && (specificRow[3].letter === duplicates[0] || specificRow[3].letter === duplicates[1] )) {
                        if ((countLetter(specificWord, duplicates[0]) === 3 || countLetter(specificWord, duplicates[1]) === 3) && ((capitalizedChosenWord[0] !==  capitalizedChosenWord[1] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[1] !== specificRow[1].letter) &&
        (capitalizedChosenWord[0] !== capitalizedChosenWord[2] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[2] !== specificRow[2].letter) &&
        (capitalizedChosenWord[0] !== capitalizedChosenWord[4] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[4] !== specificRow[4].letter) &&
        (capitalizedChosenWord[1] !== capitalizedChosenWord[2] || capitalizedChosenWord[1] !== specificRow[1].letter || capitalizedChosenWord[2] !== specificRow[2].letter) &&
        (capitalizedChosenWord[1] !== capitalizedChosenWord[4] || capitalizedChosenWord[1] !== specificRow[1].letter || capitalizedChosenWord[4] !== specificRow[4].letter) &&
        (capitalizedChosenWord[2] !== capitalizedChosenWord[4] || capitalizedChosenWord[2] !== specificRow[2].letter || capitalizedChosenWord[4] !== specificRow[4].letter)) &&
        ((specificRow[3].letter !== specificRow[0].letter && specificRow[3].letter !== specificRow[1].letter) ||
        (specificRow[3].letter !== specificRow[0].letter && specificRow[3].letter !== specificRow[2].letter) ||
        (specificRow[3].letter !== specificRow[1].letter && specificRow[3].letter !== specificRow[2].letter))) {
                            specificRow[3].color = "yellow";
                        } else if (specificRow[3].letter === duplicates[0] && (countLetter(specificWord, duplicates[0]) === 1 || countLetter(specificWord, duplicates[0]) === 2)) {
                            specificRow[3].color = "yellow";
                        } else if (specificRow[3].letter === duplicates[1] && (countLetter(specificWord, duplicates[1]) === 1 || countLetter(specificWord, duplicates[1]) === 2)) {
                            specificRow[3].color = "yellow";
                        }
                    }
                } else if (cellFlipTime === 4) {
                    if (specificRow[4].letter === capitalizedChosenWord[k] && specificRow[4].color !== "green" && (specificRow[4].letter === duplicates[0] || specificRow[4].letter === duplicates[1] )) {
                        if ((countLetter(specificWord, duplicates[0]) === 3 || countLetter(specificWord, duplicates[1]) === 3) && ((capitalizedChosenWord[0] !==  capitalizedChosenWord[1] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[1] !== specificRow[1].letter) &&
        (capitalizedChosenWord[0] !== capitalizedChosenWord[2] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[2] !== specificRow[2].letter) &&
        (capitalizedChosenWord[0] !== capitalizedChosenWord[3] || capitalizedChosenWord[0] !== specificRow[0].letter || capitalizedChosenWord[3] !== specificRow[3].letter) &&
        (capitalizedChosenWord[1] !== capitalizedChosenWord[2] || capitalizedChosenWord[1] !== specificRow[1].letter || capitalizedChosenWord[2] !== specificRow[2].letter) &&
        (capitalizedChosenWord[1] !== capitalizedChosenWord[3] || capitalizedChosenWord[1] !== specificRow[1].letter || capitalizedChosenWord[3] !== specificRow[3].letter) &&
        (capitalizedChosenWord[2] !== capitalizedChosenWord[3] || capitalizedChosenWord[2] !== specificRow[2].letter || capitalizedChosenWord[3] !== specificRow[3].letter)) &&
        ((specificRow[4].letter !== specificRow[0].letter && specificRow[4].letter !== specificRow[1].letter) ||
        (specificRow[4].letter !== specificRow[0].letter && specificRow[4].letter !== specificRow[2].letter) ||
        (specificRow[4].letter !== specificRow[0].letter && specificRow[4].letter !== specificRow[0].letter) ||
        (specificRow[4].letter !== specificRow[1].letter && specificRow[4].letter !== specificRow[2].letter) ||
        (specificRow[4].letter !== specificRow[1].letter && specificRow[4].letter !== specificRow[3].letter) ||
        (specificRow[4].letter !== specificRow[2].letter && specificRow[4].letter !== specificRow[3].letter))) {
                            specificRow[4].color = "yellow";
                        } else if (specificRow[4].letter === duplicates[0] && (countLetter(specificWord, duplicates[0]) === 1 || countLetter(specificWord, duplicates[0]) === 2)) {
                            specificRow[4].color = "yellow";
                        } else if (specificRow[4].letter === duplicates[1] && (countLetter(specificWord, duplicates[1]) === 1 || countLetter(specificWord, duplicates[1]) === 2)) {
                            specificRow[4].color = "yellow";
                        }
                    }
                }
            }
            }
            
            
            // determine which cells should be gray
            if (specificRow[i].color !== "green" && specificRow[i].color !== "yellow") {
                allCellsColor[currentTry][i].color = "gray";
            }
            
            
            // determine the colors of the keys on the keyboard
            for (var j = 0; j < keys.length; j++) {
                if (keys[j].letter === specificRowLetter && specificRowLetter === theChosenWord[i].toUpperCase()) {
                    keys[j].color = "green"; // turn key green
                } else if ((keys[j].letter === specificRow[0].letter || keys[j].letter === specificRow[1].letter || keys[j].letter === specificRow[2].letter || keys[j].letter === specificRow[3].letter || keys[j].letter === specificRow[4].letter) && keys[j].letter === theChosenWord[i].toUpperCase() && keys[j].color !== "green") {
                    keys[j].color = "yellow"; // turn key yellow
                } else if (keys[j].letter === specificRow[i].letter && keys[j].letter !== theChosenWord[i].toUpperCase() && keys[j].color !== "yellow" && keys[j].color !== "green") {
                    keys[j].color = "gray"; // turn key gray
                }
            }
            
        }
        
        // if all of the letters are correct, you won!
        if (word1.toUpperCase() === theChosenWord.toUpperCase() || word2.toUpperCase() === theChosenWord.toUpperCase() || word3.toUpperCase() === theChosenWord.toUpperCase() || word4.toUpperCase() === theChosenWord.toUpperCase() || word5.toUpperCase() === theChosenWord.toUpperCase() || word6.toUpperCase() === theChosenWord.toUpperCase()) {
            gameOver = true; // the game is over
            win = true; // display the win scene
        }
        
        // if it's the last try & the word is incorrect, you lost
        if (currentTry === 5 && word6.toUpperCase() !== theChosenWord.toUpperCase()) {
            gameOver = true; // the game is over
            lose = true; // display the lose scene
        }
        
    } else {
        cellFlipTime = 0; // "cellFlipTime" back to initial value
    }
} // "play" scene
function navigationBar() {
    
    noStroke();
    fill(whiteColor);
    rect(300, 35, 700, 70);
    
    stroke(lightGrayColor);
    strokeWeight(2);
    line(0, 70, 600, 70);
    
    PickleLogoButton.draw();
    
    RestartButton.draw();
    
    HowToPlayButton.draw();
    
    StatisticsButton.draw();
    
    SettingsButton.draw();
    
} // navigation bar
function thumbnail() {
    
    background(whiteColor); // white background
    
    PickleLogo(300, 70, 2); // the Pickle logo
    
    // draw the cell tiles
    pushMatrix();
    translate(-87.5, 15); // shift the game
    scale(1.29); // scale the game up
    
    // draw row one of cells
    for (var i = 0; i < cells1.length ; i++) {
        cells1[i].draw();
        cells1[i].letter = rawInputText[i];
    }
    
    // draw row two of cells
    for (var i = 0; i < cells2.length; i++) {
        cells2[i].draw();
        cells2[i].letter = rawInputText[i+5];
    }
    
    // draw row three of cells
    for (var i = 0; i < cells3.length; i++) {
        cells3[i].draw();
        cells3[i].letter = rawInputText[i+10];
    }
    
    // draw row four of cells
    for (var i = 0; i < cells4.length; i++) {
        cells4[i].draw();
        cells4[i].letter = rawInputText[i+15];
    }
    
    // draw row five of cells
    for (var i = 0; i < cells5.length; i++) {
        cells5[i].draw();
        cells5[i].letter = rawInputText[i+20];
    }
    
    // draw row six of cells
    for (var i = 0; i < cells6.length; i++) {
        cells6[i].draw();
        cells6[i].letter = rawInputText[i+25];
    }
    popMatrix();
    
} // thumbnail



// DRAW FUNCTION ================================================
draw = function() {
    
    cursor("default"); // default cursor
    
    background(whiteColor); // white background
    
    playScene(); // draw the cell tiles and keyboard
    
    
    // calculate the percent of games won
    if (statsGamesPlayed === 0) {
        statsPercentWon = 0 + "%";
    } else {
        statsPercentWon = round(statsGamesWon/statsGamesPlayed*100) + "%";
    }
    
    // calculate the current streak
    statsCurrentStreak = calculateCurrentStreak(streakArray);
    
    // calculate the max streak
    statsMaxStreak = calculateMaxStreak(streakArray);
    
    
    // during start-up increase the number of keys
    if (intro === false && keyAmount < keys.length) {
        keyAmount++;
    }
    
    // if restarted, move the cell tiles left and return at right
    if (moveCells) {
        moveCellsTime++; // time during moving of the cells
        if (cellsX > -500) {
            cellsX-=abs(cellsX/10)+10;
        } else if (cellsX < -500) {
            cellsX = 500;
        }
        if (cellsX < 0 && moveCellsTime > 20) {
            moveCells = false;
            cellsX = 0;
            moveCellsTime = 0;
        }
    }
    
    // if pop-up page, darken background
    if (darkenBackground) {
        darkenOpacity = 100;
    } else {
        darkenOpacity = 0;
    }
    
    // show "How to play" pop-up page
    if (howToPlayStatus) {
        darkenBackground = true;
        howToPlay();
        HowToPlayButton.currentPage = true;
    } else {
        HowToPlayButton.currentPage = false;
    }
    
    // show "Statistics" pop-up page
    if (statisticsStatus) {
        darkenBackground = true;
        statistics();
        StatisticsButton.currentPage = true;
    } else {
        StatisticsButton.currentPage = false;
    }
    
    // show "Settings" pop-up page
    if (settingsStatus) {
        darkenBackground = true;
        settings();
        SettingsButton.currentPage = true;
    } else {
        SettingsButton.currentPage = false;
    }
    
    // show "Win" pop-up page
    if (winStatus) {
        darkenBackground = true;
        winPopup();
        NewGameButton.label = "New Game";
    }
    
    // show "lose" pop-up page
    if (loseStatus) {
        darkenBackground = true;
        losePopup();
        NewGameButton.label = "Try Again";
    }
    
    navigationBar(); // draw the navigation bar
    
    // make pickle confetti come falling downward
    if (confetti) {
        drawPickleConfetti(); // draw the pickle confetti
    }
    
    // start with the introduction on top of everything
    if (intro) {
        introduction();
    }
    
    // if the key "F1" is pressed, show thumbnail
    if (revealThumbnail) {
        thumbnail();
    }
    
    typeBackspaceRestrictor = rawInputText.length-1;
    
}; // draw the code



// MOUSE INTERACTION ============================================
mouseClicked = function() {
    
    // handle navigation bar buttons
    PickleLogoButton.handleMouseClick();
    RestartButton.handleMouseClick();
    HowToPlayButton.handleMouseClick();
    StatisticsButton.handleMouseClick();
    SettingsButton.handleMouseClick();
    
    if (play) {
        
        for (var i = 0; i < keys.length; i++) {
            keys[i].handleMouseClick();
        }
    } else {
        ExitButton.handleMouseClick();
        if (howToPlayStatus || statisticsStatus || settingsStatus || winStatus || loseStatus) {
            if (mouseY > 70 && (mouseX < 75 || mouseX > 525 || mouseY < 125 || mouseY > 475)) {
                play = true;
                howToPlayStatus = false;
                statisticsStatus = false;
                settingsStatus = false;
                winStatus = false;
                loseStatus = false;
            }
        }
    }
    
    if (settingsStatus) {
        DarkModeSwitch.handleMouseClick();
        ColorBlindModeSwitch.handleMouseClick();
        SoundOnOffSwitch.handleMouseClick();
    }
    
    if (settingsStatus) {
        if (LatoIsSupported) {
            if (mouseX > 347 && mouseX < 406 && mouseY > 415 && mouseY < 435) {
                _clearLogs();
                println(WordleLink);
                (function(){return this[["document"]];})().body.childNodes[0].style.height = "45px";
            }
        } else {
            if (mouseX > 357 && mouseX < 415 && mouseY > 415 && mouseY < 435) {
                _clearLogs();
                println(WordleLink);
                (function(){return this[["document"]];})().body.childNodes[0].style.height = "45px";
            }
        }
        
    } 
    
    if (winStatus || loseStatus) {
        NewGameButton.handleMouseClick();
        ViewStatisticsButton.handleMouseClick();
    }
    
}; // activate if mouse is clicked
mouseOver = function() {
    mouseOverCanvas = true; // mouse is over canvas
}; // activate if mouse is over canvas
mouseOut = function() {
    mouseOverCanvas = false; // mouse is not over canvas
}; // activate if mouse not over canvas



// KEYBOARD INTERACTION =========================================
keyPressed = function() {
    
    // if the game is going, enable typing
    if (play && gameOver === false) {
    // if the key that's pressed is the "enter" or "return" key        (more robust and compatable accross different systems)
        if (keyCode === ENTER || keyCode === RETURN) {
            incorrectSoundTime = 0; // back to initial value
            enter = true; // enter
        } else {
            enter = false; // stop entering
            incorrectSoundTime = 0; // back to initial value
        }
        
        var key5 = event.key;
        if (keyCode >= 65 && keyCode <= 90 && rawInputText.length < letterLimit && gameOver === false) {
            var letter = keyCodeToLetter[keyCode];
            rawInputText.push(letter); // add text to array
        }
        document.addEventListener('keydown', function(event) {
            if (event.keyCode === 8 && rawInputText.length > backspaceLimit && rawInputText.length > typeBackspaceRestrictor && gameOver === false) {
                rawInputText.pop(); // backspace
            }
        });
    }
    
    // if the key "F1" is pressed, show the thumbnail
    if (keyCode === 112) {
        if (revealThumbnail) {
            revealThumbnail = false; // hide thumbnail
        } else {
            revealThumbnail = true; // show thumbnail
        }
    }
    
}; // activate if key is pressed

}};

// Get the canvas that ProcessingJS will use
var canvas = document.getElementById("mycanvas"); 
// Pass the function to ProcessingJS constructor
var processingInstance = new Processing(canvas, programCode); 
