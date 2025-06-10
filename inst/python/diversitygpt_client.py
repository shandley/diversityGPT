#!/usr/bin/env python3
"""
diversityGPT Python Client

A Python client library for interacting with the diversityGPT REST API.
Enables Python users to leverage diversityGPT's universal diversity analysis.
"""

import json
import base64
import time
from typing import Dict, List, Optional, Union, Any
import requests
import pandas as pd
import numpy as np


class DiversityGPTClient:
    """
    Client for interacting with diversityGPT REST API
    
    Example:
        >>> client = DiversityGPTClient("http://localhost:8080")
        >>> result = client.analyze(otu_table, sample_data, components=["universal"])
        >>> print(result['data']['universal']['deconvolution_quality'])
    """
    
    def __init__(self, base_url: str = "http://localhost:8080", 
                 api_key: Optional[str] = None,
                 timeout: int = 300):
        """
        Initialize the diversityGPT client
        
        Args:
            base_url: Base URL of the diversityGPT API server
            api_key: Optional API key for authentication
            timeout: Request timeout in seconds
        """
        self.base_url = base_url.rstrip('/')
        self.api_key = api_key
        self.timeout = timeout
        self.headers = {'Content-Type': 'application/json'}
        
        if api_key:
            self.headers['Authorization'] = f'Bearer {api_key}'
    
    def _make_request(self, method: str, endpoint: str, 
                      data: Optional[Dict] = None) -> Dict:
        """Make HTTP request to API"""
        url = f"{self.base_url}{endpoint}"
        
        try:
            if method == "GET":
                response = requests.get(url, headers=self.headers, 
                                      timeout=self.timeout)
            elif method == "POST":
                response = requests.post(url, headers=self.headers, 
                                       json=data, timeout=self.timeout)
            elif method == "DELETE":
                response = requests.delete(url, headers=self.headers, 
                                         timeout=self.timeout)
            else:
                raise ValueError(f"Unsupported method: {method}")
            
            response.raise_for_status()
            return response.json()
            
        except requests.exceptions.RequestException as e:
            return {
                "status": "error",
                "error": {
                    "message": str(e),
                    "code": "REQUEST_FAILED"
                }
            }
    
    def health_check(self) -> Dict:
        """Check API health status"""
        return self._make_request("GET", "/health")
    
    def analyze(self, 
                otu_table: Union[pd.DataFrame, np.ndarray],
                sample_data: Optional[pd.DataFrame] = None,
                tax_table: Optional[pd.DataFrame] = None,
                components: List[str] = None,
                async_mode: bool = False) -> Dict:
        """
        Analyze microbiome data using diversityGPT
        
        Args:
            otu_table: OTU/ASV abundance table (samples x taxa)
            sample_data: Sample metadata DataFrame
            tax_table: Taxonomic classification table
            components: Analysis components to run
            async_mode: Whether to run asynchronously
            
        Returns:
            API response dictionary
        """
        if components is None:
            components = ["universal", "mechanisms", "hypotheses"]
        
        # Prepare phyloseq-like data structure
        phyloseq_data = self._prepare_phyloseq_data(
            otu_table, sample_data, tax_table
        )
        
        request_data = {
            "data": phyloseq_data,
            "components": components,
            "async": async_mode
        }
        
        result = self._make_request("POST", "/analyze", request_data)
        
        # Handle async response
        if async_mode and result.get("status") == "accepted":
            job_id = result["data"]["job_id"]
            print(f"Async job submitted: {job_id}")
            return result
        
        return result
    
    def transform(self, 
                  source_metrics: Dict[str, List[float]],
                  target_metrics: List[str],
                  transformation_matrix: Optional[Dict] = None) -> Dict:
        """
        Transform between diversity metrics
        
        Args:
            source_metrics: Dictionary of source metric values
            target_metrics: List of target metrics to predict
            transformation_matrix: Optional pre-computed transformation matrix
            
        Returns:
            API response with transformed metrics
        """
        request_data = {
            "source_metrics": source_metrics,
            "target_metrics": target_metrics
        }
        
        if transformation_matrix:
            request_data["transformation_matrix"] = transformation_matrix
        
        return self._make_request("POST", "/transform", request_data)
    
    def validate(self, 
                 universal_info: Dict,
                 phyloseq_data: Dict,
                 validation_type: str = "full") -> Dict:
        """
        Validate universal analysis results
        
        Args:
            universal_info: Universal information object
            phyloseq_data: Original phyloseq data
            validation_type: "full" or "quick"
            
        Returns:
            Validation results
        """
        request_data = {
            "universal_info": universal_info,
            "phyloseq_data": phyloseq_data,
            "validation_type": validation_type
        }
        
        return self._make_request("POST", "/validate", request_data)
    
    def batch_analyze(self, 
                      datasets: List[Dict],
                      dataset_names: Optional[List[str]] = None,
                      analysis_steps: Optional[List[str]] = None) -> Dict:
        """
        Analyze multiple datasets in batch
        
        Args:
            datasets: List of dataset dictionaries
            dataset_names: Optional names for datasets
            analysis_steps: Analysis steps to perform
            
        Returns:
            Batch job response (always async)
        """
        request_data = {
            "datasets": datasets,
            "dataset_names": dataset_names,
            "analysis_steps": analysis_steps or ["universal", "mechanisms"]
        }
        
        return self._make_request("POST", "/batch", request_data)
    
    def meta_analysis(self, 
                      studies: List[Dict],
                      study_names: Optional[List[str]] = None,
                      method: str = "random_effects") -> Dict:
        """
        Perform meta-analysis across studies
        
        Args:
            studies: List of study data or results
            study_names: Optional study names
            method: Meta-analysis method
            
        Returns:
            Meta-analysis results
        """
        request_data = {
            "studies": studies,
            "study_names": study_names,
            "method": method
        }
        
        return self._make_request("POST", "/meta-analysis", request_data)
    
    def generate_report(self, 
                        analysis_results: Dict,
                        format: str = "html",
                        template: str = "research",
                        return_file: bool = False) -> Dict:
        """
        Generate analysis report
        
        Args:
            analysis_results: Complete analysis results
            format: Output format (html/pdf)
            template: Report template
            return_file: Whether to return file content
            
        Returns:
            Report generation response
        """
        request_data = {
            "analysis_results": analysis_results,
            "format": format,
            "template": template,
            "return_file": return_file
        }
        
        return self._make_request("POST", "/report", request_data)
    
    def search_literature(self, 
                          query: Optional[str] = None,
                          universal_info: Optional[Dict] = None,
                          databases: List[str] = None,
                          max_papers: int = 10) -> Dict:
        """
        Search scientific literature
        
        Args:
            query: Custom search query
            universal_info: Universal information for context
            databases: Literature databases to search
            max_papers: Maximum papers to return
            
        Returns:
            Literature search results
        """
        request_data = {
            "query": query,
            "universal_info": universal_info,
            "databases": databases or ["biorxiv"],
            "max_papers": max_papers
        }
        
        return self._make_request("POST", "/literature", request_data)
    
    def job_status(self, job_id: str) -> Dict:
        """Check status of async job"""
        return self._make_request("GET", f"/jobs/{job_id}")
    
    def get_results(self, job_id: str) -> Dict:
        """Get results of completed job"""
        return self._make_request("GET", f"/jobs/{job_id}/results")
    
    def cancel_job(self, job_id: str) -> Dict:
        """Cancel running job"""
        return self._make_request("DELETE", f"/jobs/{job_id}")
    
    def list_jobs(self, status_filter: Optional[str] = None) -> Dict:
        """List all jobs"""
        endpoint = "/jobs"
        if status_filter:
            endpoint += f"?status={status_filter}"
        return self._make_request("GET", endpoint)
    
    def wait_for_job(self, job_id: str, 
                     check_interval: int = 5,
                     max_wait: int = 3600) -> Dict:
        """
        Wait for async job to complete
        
        Args:
            job_id: Job ID to monitor
            check_interval: Seconds between status checks
            max_wait: Maximum seconds to wait
            
        Returns:
            Final job results or status
        """
        start_time = time.time()
        
        while time.time() - start_time < max_wait:
            status = self.job_status(job_id)
            
            if status.get("status") == "error":
                return status
            
            job_status = status.get("data", {}).get("status")
            
            if job_status in ["completed", "failed", "timeout", "cancelled"]:
                if job_status == "completed":
                    return self.get_results(job_id)
                else:
                    return status
            
            print(f"Job {job_id} status: {job_status}")
            time.sleep(check_interval)
        
        return {
            "status": "error",
            "error": {
                "message": f"Timeout waiting for job {job_id}",
                "code": "WAIT_TIMEOUT"
            }
        }
    
    def _prepare_phyloseq_data(self, 
                               otu_table: Union[pd.DataFrame, np.ndarray],
                               sample_data: Optional[pd.DataFrame] = None,
                               tax_table: Optional[pd.DataFrame] = None) -> Dict:
        """Convert Python data to phyloseq-like format"""
        
        # Convert OTU table
        if isinstance(otu_table, np.ndarray):
            otu_df = pd.DataFrame(otu_table)
        else:
            otu_df = otu_table
        
        phyloseq_data = {
            "otu_table": {
                "data": otu_df.to_dict(orient='list'),
                "taxa_are_rows": False  # Assuming samples x taxa format
            }
        }
        
        # Add sample data if provided
        if sample_data is not None:
            phyloseq_data["sample_data"] = sample_data.to_dict(orient='list')
        
        # Add taxonomy table if provided
        if tax_table is not None:
            phyloseq_data["tax_table"] = tax_table.to_dict(orient='list')
        
        return phyloseq_data


class DiversityGPTAnalysis:
    """
    High-level wrapper for diversityGPT analysis workflows
    
    Example:
        >>> analysis = DiversityGPTAnalysis(client)
        >>> results = analysis.complete_analysis(otu_table, sample_data)
        >>> analysis.plot_results(results)
    """
    
    def __init__(self, client: DiversityGPTClient):
        """Initialize with a client instance"""
        self.client = client
    
    def complete_analysis(self, 
                          otu_table: pd.DataFrame,
                          sample_data: Optional[pd.DataFrame] = None,
                          tax_table: Optional[pd.DataFrame] = None,
                          include_validation: bool = True,
                          generate_report: bool = True) -> Dict:
        """
        Run complete diversityGPT analysis pipeline
        
        Args:
            otu_table: OTU/ASV abundance table
            sample_data: Sample metadata
            tax_table: Taxonomic classifications
            include_validation: Whether to validate results
            generate_report: Whether to generate report
            
        Returns:
            Complete analysis results
        """
        print("Starting diversityGPT analysis...")
        
        # Step 1: Universal analysis
        print("Step 1: Extracting universal information...")
        result = self.client.analyze(
            otu_table, sample_data, tax_table,
            components=["universal", "mechanisms", "hypotheses"]
        )
        
        if result.get("status") == "error":
            raise Exception(f"Analysis failed: {result['error']['message']}")
        
        analysis_data = result.get("data", {})
        
        # Step 2: Validation (optional)
        if include_validation and "universal" in analysis_data:
            print("Step 2: Validating results...")
            phyloseq_data = self.client._prepare_phyloseq_data(
                otu_table, sample_data, tax_table
            )
            
            validation = self.client.validate(
                analysis_data["universal"],
                phyloseq_data,
                validation_type="quick"
            )
            
            if validation.get("status") == "success":
                analysis_data["validation"] = validation.get("data")
        
        # Step 3: Generate report (optional)
        if generate_report:
            print("Step 3: Generating report...")
            report = self.client.generate_report(
                analysis_data,
                format="html",
                template="summary"
            )
            
            if report.get("status") == "success":
                print(f"Report generated: {report['data']['report_path']}")
        
        print("Analysis complete!")
        return analysis_data
    
    def compare_datasets(self, 
                         datasets: List[pd.DataFrame],
                         dataset_names: List[str],
                         sample_metadata: Optional[List[pd.DataFrame]] = None) -> Dict:
        """
        Compare multiple datasets using batch processing
        
        Args:
            datasets: List of OTU tables
            dataset_names: Names for each dataset
            sample_metadata: Optional metadata for each dataset
            
        Returns:
            Batch comparison results
        """
        print(f"Comparing {len(datasets)} datasets...")
        
        # Prepare datasets
        prepared_datasets = []
        for i, otu_table in enumerate(datasets):
            sample_data = sample_metadata[i] if sample_metadata else None
            prepared_datasets.append(
                self.client._prepare_phyloseq_data(otu_table, sample_data)
            )
        
        # Submit batch job
        batch_result = self.client.batch_analyze(
            prepared_datasets,
            dataset_names
        )
        
        if batch_result.get("status") == "accepted":
            job_id = batch_result["data"]["job_id"]
            print(f"Batch job submitted: {job_id}")
            
            # Wait for completion
            final_result = self.client.wait_for_job(job_id)
            
            if final_result.get("status") == "success":
                return final_result.get("data")
        
        return batch_result
    
    def transform_metrics(self, 
                          metrics_df: pd.DataFrame,
                          source_column: str,
                          target_metrics: List[str]) -> pd.DataFrame:
        """
        Transform diversity metrics in a DataFrame
        
        Args:
            metrics_df: DataFrame with diversity metrics
            source_column: Column name of source metric
            target_metrics: List of target metrics to predict
            
        Returns:
            DataFrame with predicted metrics
        """
        source_data = {source_column: metrics_df[source_column].tolist()}
        
        result = self.client.transform(source_data, target_metrics)
        
        if result.get("status") == "success":
            predictions = result["data"]["predictions"]
            
            # Add predictions to dataframe
            for metric in target_metrics:
                if metric in predictions:
                    metrics_df[f"{metric}_predicted"] = predictions[metric]
            
            # Add quality metrics
            quality = result["data"].get("quality", {})
            for metric, r2 in quality.items():
                print(f"{metric} R²: {r2:.3f}")
        
        return metrics_df


# Example usage functions
def example_basic_usage():
    """Basic usage example"""
    # Initialize client
    client = DiversityGPTClient("http://localhost:8080")
    
    # Create sample data
    np.random.seed(42)
    otu_table = pd.DataFrame(
        np.random.poisson(5, size=(20, 100)),
        index=[f"Sample{i}" for i in range(20)],
        columns=[f"OTU{i}" for i in range(100)]
    )
    
    sample_data = pd.DataFrame({
        'Treatment': ['Control'] * 10 + ['Treatment'] * 10,
        'Timepoint': [1, 1, 2, 2, 3, 3, 4, 4, 5, 5] * 2
    }, index=otu_table.index)
    
    # Run analysis
    result = client.analyze(otu_table, sample_data)
    
    if result.get("status") == "success":
        print("Analysis successful!")
        print(f"Components analyzed: {list(result['data'].keys())}")
        
        # Extract results
        universal_info = result['data'].get('universal', {})
        if 'deconvolution_quality' in universal_info:
            quality = universal_info['deconvolution_quality']
            print(f"Transformation quality: {quality.get('overall_quality')}")


def example_async_batch():
    """Async batch processing example"""
    client = DiversityGPTClient("http://localhost:8080")
    analysis = DiversityGPTAnalysis(client)
    
    # Create multiple datasets
    datasets = []
    for i in range(3):
        np.random.seed(i)
        otu_table = pd.DataFrame(
            np.random.poisson(10, size=(15, 50)),
            index=[f"S{j}" for j in range(15)],
            columns=[f"OTU{j}" for j in range(50)]
        )
        datasets.append(otu_table)
    
    # Compare datasets
    results = analysis.compare_datasets(
        datasets,
        dataset_names=["Environment1", "Environment2", "Environment3"]
    )
    
    print("Batch analysis complete!")
    

if __name__ == "__main__":
    # Run examples if executed directly
    print("diversityGPT Python Client Examples")
    print("===================================")
    
    try:
        example_basic_usage()
    except Exception as e:
        print(f"Basic example failed: {e}")
    
    print("\n")
    
    try:
        example_async_batch()
    except Exception as e:
        print(f"Batch example failed: {e}")