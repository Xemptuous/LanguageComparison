type Point = { x: number; y: number };

function greet(name: string): void {
    console.log(`Hi, ${name}!`);
}

function square(n: number): number {
    return n * n;
}

const name = "Alice";
const x = 5;
const y = 3.14;
const active = true;

greet(name);
console.log(`Square of ${x} is ${square(x)}`);

const p: Point = { x: 3, y: 4 };
console.log(`(${p.x}, ${p.y})`);

const nums = [1, 2, 3];
nums.forEach(n => process.stdout.write(n + " "));
console.log();

const ages: Record<string, number> = { Alice: 30, Bob: 25 };
console.log(`Bob is ${ages["Bob"]} years old.`);

let count = 0;
while (count < 3) {
    console.log(`While loop: ${count}`);
    count++;
}

