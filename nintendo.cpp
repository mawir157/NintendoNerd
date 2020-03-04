#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

#include <iomanip>

std::string intToHexString(const uint32_t n)
{
  std::stringstream stream;
  stream << std::hex << std::setw(8) << std::setfill('0') << n;
  std::string result( stream.str() );

  return result;
}


uint32_t hexStringToInt(const std::string s)
{
  uint32_t x;
  std::stringstream ss;
  ss << std::hex << s;
  ss >> x;

  return x;
}


// input = 01234567 890abcdef 01234567 890abcdef
std::vector<uint32_t> parseCLI(const std::string line)
{
  std::vector<uint32_t> v;
  int cursor = 0;
  int space = line.find(" ", cursor);
  while (space >= 0)
  {
    std::string hexString = line.substr(cursor, space);
    v.push_back(hexStringToInt(hexString));
    cursor = space + 1;
    space = line.find(" ", cursor);
  }
  // need to do it one more time to get final element of list
  std::string hexString = line.substr(cursor, 100000);
  v.push_back(hexStringToInt(hexString));

  return v;
}


typedef std::vector<uint8_t> f2Poly;
typedef std::pair<f2Poly, f2Poly> polyPair;


void trimPoly(f2Poly& v)
{
  while (!v.empty() &&  v[v.size() - 1] == 0)
    v.pop_back();
}


void trimPolyL(f2Poly& v)
{
  std::reverse(v.begin(), v.end());

  while (!v.empty() &&  v[v.size() - 1] == 0)
    v.pop_back(); 

  std::reverse(v.begin(), v.end());
}


f2Poly parseInput(const std::vector<uint32_t> v, const bool trim = true)
{
  f2Poly r;
  for (size_t vi = 0; vi < v.size(); ++vi)
    for (size_t bi = 0; bi < 32; ++bi)
      r.push_back((v[vi] >> bi) & 1);

  // remove `trailing' zeroes
  if (trim)
    trimPoly(r);

  std::reverse(r.begin(), r.end());
  return r;
}


std::vector<uint32_t> deparse(f2Poly f)
{
  std::reverse(f.begin(), f.end());

  size_t array_size = 1 + (f.size() - 1)/32;

  std::vector<uint32_t> r(array_size);
  for (size_t i = 0; i < f.size(); ++i)
    r[i/32] += ((1 & f[i]) << (i % 32));

  std::reverse(f.begin(), f.end());

  return r;
}


void vecXOR(f2Poly& a, const f2Poly b, size_t shift = 0)
{
  for (size_t i = 0; i < b.size(); ++i)
    a[i + shift] ^= b[i];
}


bool allZero(const f2Poly f)
{
  for (size_t i = 0; i < f.size(); ++i)
    if (f[i] != 0)
      return false;

  return true;
}


f2Poly zeroPad(const f2Poly f, const size_t new_length = 32)
{
  if (f.size() >= new_length)
    return f;

  f2Poly r;

  for (size_t i = 0; i < new_length - f.size(); ++i)
    r.push_back(0);

  for (size_t i = 0; i < f.size(); ++i)
    r.push_back(f[i]);

  return r;
}


// a += b
f2Poly polyAdd(const f2Poly a, const f2Poly b)
{
  f2Poly r;
  if (a.size() > b.size())
  {
    r = a;
    vecXOR(r, b, a.size() - b.size());
  }
  else
  {
    r = b;
    vecXOR(r, a, b.size() - a.size());   
  }
  return r;
}


// f = h*g + r
polyPair polyDiv(const f2Poly f,  const f2Poly g)
{
  f2Poly h;
  const size_t fn = f.size();
  const size_t gn = g.size();
  unsigned int loop = (fn < gn) ? 0 : fn - gn;
  f2Poly r = f;

  for (size_t i = 0; i <= loop; ++i)
  {
    if (r[i] == 1)
    {
      vecXOR(r, g, i);
      h.push_back(1);
      if (allZero(r))
        break;
    }
    else
      h.push_back(0);
  }
  trimPolyL(r);
  trimPoly(h);

  polyPair div(h, r);
  return div;
}


// function gcd(a, b)
//     if b = 0
//         return a
//     else
//         return gcd(b, a mod b)
f2Poly GCD(const f2Poly f, const f2Poly g, size_t depth = 0)
{
  if (depth > 10000)
    return f;

  if (g.size() == 0)
    return f;
  else
  {
    std::pair<f2Poly, f2Poly> k = polyDiv(f, g);
    return GCD(g, k.second, depth + 1);
  }
}


f2Poly polynomialDiff(const f2Poly f)
{
  f2Poly d;
  for (size_t i = 1; i < f.size(); ++i) 
  {
    if (i % 2 == 1)
      d.push_back(f[i]);
    else
      d.push_back(0);
  }

  trimPoly(d);

  return d;
}


// returns g mod f
f2Poly polyMod(const f2Poly g, const f2Poly f) {
  if (g.size() < f.size())
    return g;

  // f = h*g + r
  std::pair<f2Poly, f2Poly> k =  polyDiv(g, f);

  return k.second;
}


f2Poly nthPow(const size_t n, bool squared = false)
{
  f2Poly r((squared ? 2 : 1) * n);
  r.push_back(1);

  std::reverse(r.begin(), r.end());
  return r;
}

std::vector<f2Poly> identity(const size_t n)
{
  std::vector<f2Poly> t;
  for (size_t i = 0; i < n; ++i)
  {
    f2Poly r(i);
    r.push_back(1);
    for (size_t j = 0; j < n - i-1; ++j)
      r.push_back(0);

    t.push_back(r);
  }
  return t;
}


size_t firstOne(const f2Poly f)
{
  for (size_t i = 0; i < f.size(); ++i)
    if (f[i] == 1)
      return i;

  return -1;
}


void cref(std::vector<f2Poly>& rows, std::vector<f2Poly>& id)
{
  for (size_t r = 0; r < rows.size(); ++r)
  {
    // step 1 Find a row with a 1 in the r-th position
    for (size_t i = 0; i < rows.size(); ++i)
    {
      // step 1 Find a row with a 1 in the r-th position
      if (firstOne(rows[i]) == r)
      {
        if (i != r) // only swap when necessary
        {
          iter_swap(rows.begin() + i, rows.begin() + r);
          iter_swap(id.begin() + i, id.begin() + r);
        }

        break;
      }
    }
    // step 2 Add the moved row to all OTHER
    // rows with a 1 in the r-th position
    for (size_t j = 0; j < rows.size(); ++j)
    {
      if (rows[j][r] == 1)
      {
        if (j != r)
        {
          vecXOR(rows[j], rows[r]);
          vecXOR(id[j], id[r]);
        }
      }      
    }
  }
}


std::vector<f2Poly> nullSpace(const std::vector<f2Poly>& rows,
                                 const std::vector<f2Poly>& id)
{
  std::vector<f2Poly> nullity;

  for (size_t r = 0; r < rows.size(); ++r)
  {
    if (allZero(rows[r]))
    {
      f2Poly temp = id[r];
      trimPoly(temp);
      std::reverse(temp.begin(), temp.end());
      nullity.push_back(temp);
    }
  }

  return nullity;
}


// we pass a in polynomial f and get back a list of pairs of polynomials 
// [(g_1, h_1),...,(g_n, h_n)] s.t. f = g_i*h_i
std::vector<polyPair> Berlekamp(const f2Poly f, const size_t s)
{
  size_t max_bits = f.size() - 1;

  std::vector<f2Poly> columns;
  for (size_t i = 0; i < max_bits; ++i)
  {
    f2Poly x_i     = nthPow(i, false);
    f2Poly x_i_sqd = nthPow(i, true);

    // col = (x^n)^2 - (x^n) mod f
    f2Poly col = polyMod(polyAdd(x_i, x_i_sqd), f);
    col = zeroPad(col, max_bits);

    columns.push_back(col);
  }

  std::vector<f2Poly> idty = identity(columns.size()); 
  cref(columns, idty);
  std::vector<f2Poly> nullity = nullSpace(columns, idty);

  std::vector<polyPair> pairs;
  
  for (size_t i = 0; i < nullity.size(); ++i)
  {
    f2Poly p1 = GCD(f, nullity[i]);
    
    vecXOR(nullity[i], nthPow(0, false), nullity[i].size() - 1);
    f2Poly p2 = GCD(f, nullity[i]);
    std::pair<f2Poly, f2Poly> p(p1, p2);

    if ((p1.size() <= s) && (p2.size() <= s))
      pairs.push_back(p);
  }

  std::sort (pairs.begin(), pairs.end(),
             [](polyPair f, polyPair g) { return f.first.size() > g.first.size(); });
  
  return pairs;
}

int main()
{
  size_t S;
  std::cin >> S; std::cin.ignore();
  std::string line;
  getline(std::cin, line);

  std::vector<uint32_t> v_input = parseCLI(line);
  f2Poly f_new = parseInput(v_input);

  f2Poly gcd_poly = GCD(f_new, polynomialDiff(f_new));
  if (gcd_poly.size() != 1)
  {
    std::cout << "This polynomial has a repeated factor" << std::endl;
    // we could add an extra bit of logic here to remove the repeated factors
    // then put them back at the end. But none of the examples require it.
  }

  std::vector<polyPair> pairs = Berlekamp(f_new, S);

  std::vector<std::string> solution_strings;

  for (size_t i = 0; i < pairs.size(); ++i)
  {
    f2Poly p1 = pairs[i].first;
    f2Poly p2 = pairs[i].second;

    std::string s1;
    std::string s2;

    std::vector<uint32_t> d_p1 = deparse(p1);
    std::vector<uint32_t> d_p2 = deparse(p2);

    if (d_p1.size() != d_p2.size())
      std::cout << "different sized deparsed inputs!" << std::endl;

    size_t blocks = d_p1.size();
    
    for (size_t j = 0; j < blocks; ++j)
    {
      s1.append(intToHexString(d_p1[j]));
      s1.append(" ");

      s2.append(intToHexString(d_p2[j]));
      s2.append(" ");    
    }

    for (size_t j = 0; j < blocks; ++j)
    {
      s1.append(intToHexString(d_p2[j]));
      s2.append(intToHexString(d_p1[j]));

      if (j != blocks - 1)
      {
        s1.append(" ");
        s2.append(" "); 
      }
    }
    solution_strings.push_back(s1);
    solution_strings.push_back(s2);
  }

  std::sort(solution_strings.begin(), solution_strings.end());

  for (size_t i = 0; i < solution_strings.size(); ++i)
    std::cout << solution_strings[i] << std::endl;
  
  return 0;
}
