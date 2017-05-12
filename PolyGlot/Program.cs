using System;
using Puzzle;

namespace PolyGlot
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("***** Want a puzzle? Press y/n *****\n");
            var continu = Console.ReadKey();
            while (continu.KeyChar == 'y')
            {
                //int[][] puzzle = Generate.newPuzzle(5, 5);
                int[][] puzzle = Generate.newPuzzleOfScore(40, 5, 5);
                PrintArrayOfArray(puzzle);
                Console.WriteLine("\n***** Another one? Press y/n *****\n");
                continu = Console.ReadKey();
            }
        }

        public static void PrintArrayOfArray<T>(T[][] puzzle)
        {
            for (int i = 0; i < puzzle.GetLength(0); i++)
            {
                for (int j = 0; j < puzzle.GetLength(0); j++)
                    Console.Write("\t" + puzzle[i][j]);
                Console.WriteLine();
            }
        }
    }
}
