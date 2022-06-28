#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>

constexpr int MAX = 9999;

bool flunc(int x)
{
    for (size_t i = 3; i < std::sqrt(MAX); i += 2)
    {
        if (x % i == 0 && x != i)
        {
            return false;
        }
    }
    return true;
}

int main()
{
    std::vector<int> array = {2, 3};

    for (size_t i = 2; i < int((MAX * MAX - 1) / 6); i++)
    {
        double k = std::sqrt((double)(6 * i + 1));
        if (int(k) == k && flunc(k))
        {
            array.push_back(k);
        }
    }

    for (auto &&i : array)
    {
        std::cout << i << ' ';
    }
}