# Q1
# 1.	Give the names of employees, 
# whose salaries are greater than their immediate managers’:

SELECT m_employee.Name
FROM Employee AS m_employee
INNER JOIN Employee AS m_manager
  ON m_employee.manager_id = m_manager.id
WHERE m_employee.salary > m_manager.salary
ORDER BY m_employee.Name;


# Q2
# 1.	What is the average salary of employees 
# who do not manage anyone? 

SELECT AVG(m_employee2.salary) 
FROM Employee AS m_employee1
RIGHT JOIN Employee AS m_employee2
  ON m_employee1.manager_id = m_employee2.id
WHERE m_employee1.manager_id is NULL;


